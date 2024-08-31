<div align="center" style="margin-top: 40px;">
    <img src="./eipllogo.png" alt="Explain in Plain Language Autograder" style="width: 300px; border-radius: 8px; margin-bottom: 20px;">
    <h5 style="font-size: 1.1em; color: #777; margin-bottom: 20px;">An automatic grading suite for "Explain in Plain Language" questions.</h5>

  <a href="https://pypi.python.org/pypi/eiplgrader">
    <img src="https://img.shields.io/pypi/v/eiplgrader.svg" alt="Version">
  </a>
  <a href="https://pypi.python.org/pypi/eiplgrader">
    <img src="https://img.shields.io/pypi/l/eiplgrader.svg" alt="License">
  </a>
  <a href="https://pypi.python.org/pypi/eiplgrader">
    <img src="https://img.shields.io/pypi/pyversions/eiplgrader.svg" alt="Supported Python versions">
  </a>
</div>


---

## Installation and Usage


## Usage

Detailed used can be found in the [eiplgrader documentation](https://hamiltonfour.tech/eiplgrader/).

#### RECOMMENDED: Docker Usage

Given that, at its core, an `eiplquestion` asks students to prompt a model
which then generates arbitrary code it is **_highly recommended_** that this
code only be run in a sandboxed environment. To aid in this, this package 
is also published on [dockerhub](). Its usage is as follows:
```bash
# under development
```

#### Python Usage

In the event you are already running this code in a sandboxed environment you
may opt instead to simply use the python package to develop autograders
independently of the provided docker image. The package is published on 
[pypi](https://pypi.org/project/eiplgrader/) and can be installed using pip.

```bash
pip install eiplgrader
```

An example script is provided in
the
[example.py](https://github.com/CoffeePoweredComputers/eiplgrader/blob/master/example.py)
script in the repository.


```
#
# example.py
#

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester

code_generator = CodeGenerator(
        "YOUR API KEY HERE"
        )
generated_code = code_generator.generate_code("that adds two numbers.")

# define your tests

test_cases = [
    [[1, 2], 3],
    [[-1, 1], 0],
    [[-1, -1], -2]
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()
```


## Cite
When using this tool, please cite the following paper:

```bibtex
@inproceedings{smith2024code,
    author = {Smith IV, David H. and Zilles, Craig},
    title = {Code Generation Based Grading: Evaluating an Auto-grading Mechanism for "Explain-in-Plain-English" Questions},
    year = {2024},
    isbn = {9798400706004},
    publisher = {Association for Computing Machinery},
    address = {New York, NY, USA},
    url = {https://doi.org/10.1145/3649217.3653582},
    doi = {10.1145/3649217.3653582},
    booktitle = {Proceedings of the 2024 on Innovation and Technology in Computer Science Education V. 1},
    pages = {171–177},
    numpages = {7},
    keywords = {auto-grading, eipe, gpt-4, large language models},
    location = {Milan, Italy},
    series = {ITiCSE 2024}
}
```

## Research
This autograder has been evaluated and used in the following research:

- [Code Generation Based Grading: Evaluating an Auto-grading Mechanism for "Explain-in-Plain-English" Questions](https://doi.org/10.1145/3649217.3653582)
- [Explaining Code with a Purpose: An Integrated Approach for Developing Code Comprehension and Prompting Skills](https://doi.org/10.1145/3649217.3653587)
- [Prompting for Comprehension: Exploring the Intersection of Explain in Plain English Questions and Prompt Writing](https://doi.org/10.1145/3657604.3662039)
