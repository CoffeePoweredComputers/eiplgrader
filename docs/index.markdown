---
title: "Home"
layout: home
nav_order: 1
description: "eiplgrader - A modern tool for grading Explain in Plain Language (EIPL) questions"
permalink: /
---

# Welcome to eiplgrader

This is the documentation for the `eiplgrader` tool, which is used to grade Explain in Plain Language (EIPL) questions in computer science education. The tool leverages AI to provide immediate, accurate feedback on student responses.

## Core Components

The tool consists of two main modules:

- [Code Generation Module](/docs/codegen): Transforms natural language descriptions into executable Python code using OpenAI's GPT models
- [Tester Module](docs/tester): Evaluates the generated code against predefined test cases to assess correctness

## Installation

To install the `eiplgrader` tool, you can use `pip`:

```bash
pip install eiplgrader
```

## Docker Usage (Recommended)

Given this tool generates and executes arbitrary code, it is recommended to run
it in a sandboxed environment such as a Docker container. 

The package is available as a Docker image on Docker Hub. You can pull the image using the following command:

```bash
docker pull coffeepwrdcomputers/eiplgrader
```
For each question you will need to construct a `tests.json` file that 
contains the inputs and expected outputs for the test cases. An example
is provided in `example_test_cases.json`.`

To run the tool in a Docker container, you will need to provide an api key, the
prompt, and the test cases file as environment variables. You can run the tool
using the following command:
```bash
docker run -e API_KEY="<YOUR-API-KEY-HERE>" -e PROMPT="<PROMPT-HERE>" -e TEST_CASES_FILE="tests.json" -v $/path/to/tests.json:/app/<TEST-CASES-FILE> eiplgrader
```

For example, to grade a question with the following prompt and test cases from the provided example:

```bash
docker run -e API_KEY="<YOUR-API-KEY-HERE>" -e PROMPT="adds two numbers" -e TEST_CASES_FILE="example_test_cases.json" -v $(pwd)/example_test_cases.json:/app/example_test_cases.json eiplgrader
```

If you already have code generated and want to test it, you can run the following command:
```bash
docker run -e USER_CODE="<YOUR-CODE-HERE>" -e TEST_CASES_FILE="example_test_cases.json" -v $(pwd)/example_test_cases.json:/app/example_test_cases.json eiplgrader
```

From the previous example, if you have the following code generated:
```python
def foo(a, b):
    return a + b
```
Then it can be tested using the following command:
```bash
docker run -e USER_CODE="def foo(a, b): return a + b" -e TEST_CASES_FILE="example_test_cases.json" -v $(pwd)/example_test_cases.json:/app/example_test_cases.json eiplgrader
```

## Package Usage (⚠️ UNSAFE ⚠️)

To use the `eiplgrader` tool, you need to provide an API key for OpenAI's GPT model. You can get an API key by signing up on the [OpenAI website](https://platform.openai.com/).

Here is an example of how to use the `eiplgrader` tool:

```python
from eiplgrader import CodeGenerator, CodeTester

# Initialize the code generator
code_generator = CodeGenerator("YOUR API KEY HERE")

# Generate code based on the student's response
# For example, for the following code:
# def add(a, b):
#     return a + b
# The student's response could be:
# a function that adds two numbers.
# The following line generates the code based on the student's response.
generated_code = code_generator.generate_code("that adds two numbers.")

# Initialize the code tester
code_tester = CodeTester(generated_code, test_cases)

# Run the tests
test_result = code_tester.run_tests()
```

## Contributing

If you would like to contribute to the `eiplgrader` tool, please check out the [GitHub repository](https://github.com/CoffeePoweredComputers/eiplgrader) and feel free to submit a pull request.

## License

This project is licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html). You are free to use, modify, and distribute this software under the terms of the license.

## Learn More

To understand the research and educational theory behind eiplgrader, visit our [Research](/docs/research) section.

