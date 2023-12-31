from setuptools import setup, find_packages

setup(
    name='mycoloredlog',
    version='0.1',
    packages=find_packages(),
    install_requires=[
        'coloredlogs',
        'verboselogs',
        'logging',
        'dataclasses'
    ],
    author='Jorge González García',
    author_email='jorgegonzalezvet@gmail.com',
    description='A custom logging package with all the power of coloredlogs and verboselogs.',
    keywords='logging coloredlogs verboselogs custom simple'
)