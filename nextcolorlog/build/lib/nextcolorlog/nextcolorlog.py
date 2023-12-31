import logging
import verboselogs
import coloredlogs
from dataclasses import dataclass, field

@dataclass
class ColoredLogger:
    '''
    A dataclass to create a coloured logger with the following levels:
    debug, info, notice, warning, error, critical, success, verbose, spam

    The default level is SPAM, which will display all levels.
    '''
    name: str
    level: str = 'SPAM'
    level_styles: dict = field(default_factory=lambda: {
        'debug': {'color': 'green', 'bright': True},
        'info': {'color': 'blue', 'bright': True},
        'notice': {'color': 'magenta', 'bright': True},
        'warning': {'color': 'yellow', 'bright': True},
        'error': {'color': 'red', 'bright': True},
        'critical': {'bold': True, 'color': 'red', 'bright': True},
        'success': {'bold': True, 'color': 'green', 'bright': True},
        'verbose': {'color': 'blue', 'bright': True},
        'spam': {'color': 'green', 'faint': True, 'bright': True}
    })
    field_styles: dict = field(default_factory=lambda: {
        'asctime': {'color': 'white', 'bright': True, 'faint': True},
        'hostname': {'color': 'white'},
        'levelname': {'color': 'white'},
        'name': {'color': 'white'},
        'programname': {'color': 'white'},
        'username': {'color': 'white'},
        'filename': {'color': 'white'},
        'line': {'color': 'white'},
        'msecs': {'color': 'white'},
        'separator': {'color': 'white'}
    })

    def __post_init__(self):
        """
            Initialize the object after it has been created.

            This method installs the verboselogs package, sets up the logger, adds a filter, and installs coloredlogs.
            The logger is configured with a specific format and level styles.

            Args:
                self: The instance of the object.

            Returns:
                None
            """
        verboselogs.install()
        self.logger = logging.getLogger(self.name)
        self.logger.addFilter(self.SeparatorFilter())
        coloredlogs.install(level=self.level,
                            logger=self.logger,
                            fmt='%(asctime)s %(separator)s %(levelname)s %(separator)s %(message)s',
                            level_styles=self.level_styles,
                            field_styles=self.field_styles)

    class SeparatorFilter(logging.Filter):
        def filter(self, record):
            record.separator = '-'
            return True

    def log(self, level, message):
        '''
        Log a message with the specified log level.

    Args:
        level (str): The log level.
        message (str): The message to be logged.

    Returns:
        None
        '''
        if level in self.level_styles:
            getattr(self.logger, level)(message)

def mycolourlog(name=__name__, level='SPAM'):
    """
    Create a colored logger with the specified name and log level.

    Args:
        name (str, optional): The name of the logger (default: __name__).
        level (str, optional): The log level (default: 'SPAM').

    Returns:
        logging.Logger: The logger instance.
    """
    return ColoredLogger(name, level).logger

if __name__ == '__main__':
    logger = mycolourlog()
    logger.debug("This is a debug message")
    logger.info("This is an informational message")
    logger.warning("This is a warning message")
    logger.error("This is an error message")
    logger.critical("This is a critical message")
    logger.success("This is a success message")
    logger.verbose("This is a verbose message")
    logger.notice("This is a notice message")
    logger.spam("This is a spam message")
