---
title: "Home"
layout: home
nav_order: 1
description: "eiplgrader - A tool to grade the EIPL questions."
permalink: /
---

# Welcome to eiplgrader

This is the documentation for the `eiplgrader` tool, which is used to grade the EIPL questions. The tool consists of two main modules:

- [Code Generation Module](/docs/codegen): This module generates Python code using OpenAI's GPT models
- [Tester Module](docs/tester): This module tests the generated code using test cases.

## Installation

To install the `eiplgrader` tool, you can use `pip`:

```bash
pip install eiplgrader
```

## Usage

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

