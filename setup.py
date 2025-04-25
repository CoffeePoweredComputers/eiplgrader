from setuptools import setup, find_packages

setup(
    name="eiplgrader",
    version="0.1.3",
    description="An extension of the unittest with the capability to generate code based on a prompt and test the generated code for the purpose of grading Explain in Plain Language (EiPL) Questions",
    author="David H. Smith IV",
    author_email="dhsmith2@illinois.edu",
    packages=find_packages(),
    install_requires=[
        "openai",
    ],
    python_requires=">=3.10,<3.14",
    classifiers=[
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Programming Language :: Python :: 3.13",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
    ],
)
