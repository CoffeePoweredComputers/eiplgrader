from setuptools import setup, find_packages

setup(
    name='eipegrader',
    version='0.1.0',
    description='An extension of the unittest with the capability to generate code based on a prompt and test the generated code for the purpose of grading Explain in Plain Language (EiPL) Questions',
    author='David H. Smith IV',
    author_email='dhsmith2@illinois.edu',
    packages=find_packages(),
    install_requires=[
        'openai',
    ],
    python_requires='>=3.10',
)

