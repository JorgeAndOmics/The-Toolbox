options(warn = -1)

suppressMessages({
  library(argparse)
  library(dplyr)
  library(ggplot2)
  library(Biostrings)
  library(rtracklayer)
  library(plyranges)
  library(classInt)
  })


parser <- ArgumentParser(description="FASTA Sequence Length Clustering and XY/Mitochondrial Chromosome Gene Matching")

parser$add_argument("--fasta-path", type="character", required=TRUE,
                    help="Path to the input FASTA file")

parser$add_argument("--annotation-path", type="character", required=TRUE,
                    help="Path to the gene annotation file in BED format")

parser$add_argument("--clusters", type="integer", required=TRUE,
                    help="Species name for labeling")

parser$add_argument("--output-dir", type="character", required=TRUE,
                    help="Directory to save output CSV and plot")

args <- parser$parse_args()

message(paste("Processing FASTA file:", args$fasta_path))

## FIRST PART: Read FASTA and create tibble with sequence lengths
# Import data and parse
fasta_name <- tools::file_path_sans_ext(basename(args$fasta_path))


# Function to read FASTA file and return a tibble with sequence names and sequence lengths
read_fasta_lengths <- function(DNAStringSet_obj) {
  sequences <- DNAStringSet_obj
  
  # Create a tibble with sequence names and lengths
  fasta_lengths <- tibble(
    seq_name = names(sequences),
    seq_length = width(sequences),
    log_seq_length = log10(width(sequences)),
  )
  
  return(fasta_lengths)
}

# Execute function on the FASTA file
dna_set <- readDNAStringSet(args$fasta_path)
dna_set_table <- read_fasta_lengths(dna_set)


# Arrange data by sequence length and add rank column to tibble
dna_set_table_arranged <- dna_set_table %>%
  arrange(desc(seq_length)) %>%
  mutate(rank = row_number()) %>%
  mutate(species = fasta_name) %>%
  relocate(species, .before = seq_name) %>%
  relocate(rank, .before = species)


## SECOND PART: Jenks clustering

breaks <- classIntervals(
  dna_set_table_arranged$log_seq_length, 
  n = args$clusters,
  style = "jenks"
)

dna_set_classified <- dna_set_table_arranged %>%
  mutate(
    cluster = cut(log_seq_length, 
                  breaks = breaks$brks,
                  labels = FALSE,
                  include.lowest = TRUE)
  )


## THIRD PART: XY CHROMOSOME MATCHING
query_genes_path <- file.path(args$annotation_path)

query_genes <- rtracklayer::import(query_genes_path, format = "bed")

# Verify seqnames identical to fasta seq_names
identical_names <- all(as.character(seqnames(query_genes)) %in% dna_set_classified$seq_name)
print(paste("All seqnames in query genes are present in fasta sequences:", identical_names))

x_chromosome_genes <- tibble(
  gene_name = c("AMELX", "AR", "OPN1LW", "PGK1", "ATP7A"),
  ensembl_id = c("ENSG00000125363", "ENSG00000169083", 
                 "ENSG00000102076", "ENSG00000102144", 
                 "ENSG00000165240"))

y_chromosome_genes <- tibble(
  gene_name = c("SRY", "ZFY", "AMELY", "RPS4Y", "UTY", "DDX3Y", "USP9Y"),
  ensembl_id = c("ENSG00000184895", "ENSG00000067646", 
                 "ENSG00000099721", "ENSG00000213318", 
                 "ENSG00000183878", "ENSG00000067048",
                 "ENSG00000114374"))

mito_chromosome_genes <- tibble(
  gene_name = c("MT-ATP6", "COX1", "CYTB", "ND!"),
  ensembl_id = c("ENSG00000198899", "ENST00000841050", 
                 "ENSMUSG00000064370", "ENSMUSG00000064341"))

# Find matches in query_genes
x_matches <- query_genes %>% plyranges::filter(name %in% x_chromosome_genes$ensembl_id)
y_matches <- query_genes %>% plyranges::filter(name %in% y_chromosome_genes$ensembl_id)
mt_matches <- query_genes %>% plyranges::filter(name %in% mito_chromosome_genes$ensembl_id)

# Add corresponding gene names to x_matches and y_matches
x_matches_comp <- x_matches %>%
  as_tibble() %>%
  left_join(x_chromosome_genes, by = c("name" = "ensembl_id"))

y_matches_comp <- y_matches %>%
  as_tibble() %>%
  left_join(y_chromosome_genes, by = c("name" = "ensembl_id"))

mt_matches_comp <- mt_matches %>%
  as_tibble() %>%
  left_join(mito_chromosome_genes, by = c("name" = "ensembl_id"))


# Add gene matches to the classified tibble, separated by a comma
dna_set_xchrom <- dna_set_classified %>%
  left_join(
    x_matches_comp %>%
      select(seq_name = seqnames, x_gene = gene_name) %>%
      group_by(seq_name) %>%
      summarize(x_genes = paste(x_gene, collapse = ", ")),
    by = "seq_name"
  )

dna_set_xychrom <- dna_set_xchrom %>%
  left_join(
    y_matches_comp %>%
      select(seq_name = seqnames, y_gene = gene_name) %>%
      group_by(seq_name) %>%
      summarize(y_genes = paste(y_gene, collapse = ", ")),
    by = "seq_name"
  )

dna_set_mxychrom <- dna_set_xychrom %>%
  left_join(
    mt_matches_comp %>%
      select(seq_name = seqnames, mt_gene = gene_name) %>%
      group_by(seq_name) %>%
      summarize(mt_genes = paste(mt_gene, collapse = ", ")),
    by = "seq_name"
  ) %>%
  arrange(desc(seq_length))


# Visualize the results
jenks_plot <- ggplot(dna_set_mxychrom, aes(x = rank, y = log_seq_length)) +
  # Base layer: all points colored by cluster
  geom_point(data = dna_set_mxychrom %>% filter(is.na(mt_genes) & is.na(y_genes) & is.na(x_genes)), 
             aes(color = factor(cluster))) +
  # Highlight layer: chromosome-specific genes (larger, opaque)
  geom_point(data = dna_set_mxychrom %>% filter(!is.na(mt_genes)),
             color = "yellow", alpha = 1) +
  geom_point(data = dna_set_mxychrom %>% filter(!is.na(y_genes)),
             color = "magenta", alpha = 1) +
  geom_point(data = dna_set_mxychrom %>% filter(!is.na(x_genes)),
             color = "green", alpha = 1) +
  # Base layer: all points colored by cluster
  geom_point(data = dna_set_mxychrom %>% filter(is.na(mt_genes) & is.na(y_genes) & is.na(x_genes)), 
                                                aes(color = factor(cluster))) +
  
  # Force blue color scale
  scale_color_manual(
    values = c("1" = "#08519c", "2" = "#3182bd", "3" = "#6baed6"),
    name = "Jenks Cluster"
  ) +
  
  geom_hline(yintercept = breaks$brks[-c(1, length(breaks$brks))], 
             linetype = "dashed", color = "red") +
  
  labs(x = "Scaffold rank (by length)",
       y = "Length (bp, log10)",
       title = paste0(fasta_name, ". Jenks Natural Breaks Clustering"),
       subtitle = "Yellow=MT | Green=X | Magenta=Y") +
  theme_bw()

## FOURTH PART: Export results
# Export CSV with sequence lengths, clusters, and XY chromosome gene matches
output_path <- file.path(args$output_dir, paste0(fasta_name, ".csv"))
write.csv(dna_set_mxychrom, output_path, row.names = FALSE)

# Export Jenks plot
output_plot_path <- file.path(args$output_dir, paste0(fasta_name, ".png"))
ggsave(output_plot_path, jenks_plot, width = 10, height = 6)
