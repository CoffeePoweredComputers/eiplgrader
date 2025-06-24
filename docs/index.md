---
layout: default
title: Home
nav_order: 1
description: "EiplGrader - Automatic grading for 'Explain in Plain Language' questions"
permalink: /
---

# EiplGrader Documentation

<div align="center">
    <img src="/assets/images/eiplgrader.png" alt="EiplGrader Logo" style="width: 300px; margin-bottom: 20px;">
</div>

## An Automatic Grading Suite for "Explain in Plain Language" Questions

EiplGrader automatically grades programming assignments where students explain algorithms in natural language. It uses large language models to generate code from student explanations and tests that code against predefined test cases.

## 🚀 Quick Start

### Installation

```bash
pip install eiplgrader
```

### Set Your API Key

```bash
export OPENAI_API_KEY="your-api-key-here"
```

### Basic Example

```python
from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester

# Generate code from natural language
code_generator = CodeGenerator(api_key, language="python")
result = code_generator.generate_code(
    student_response="that adds two numbers and returns the result",
    function_name="add_numbers",
    gen_type="cgbg"
)

# Test the generated code
test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 3},
    {"parameters": {"a": -1, "b": 1}, "expected": 0}
]

code_tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name="add_numbers",
    language="python"
)

test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")
```

## 🎯 Key Features

- **Multi-language Support**: Python, JavaScript, Java, C++, C, Go, and Haskell
- **Intelligent Type Inference**: Automatic type detection for dynamic languages
- **Multiple Generation Modes**: CGBG, function redefinition, and code segmentation
- **Comprehensive Testing**: Built-in test runner with detailed results
- **Research-backed**: Based on peer-reviewed educational research
- **Flexible Architecture**: Easy to extend with new languages and features

## 📚 Documentation Sections

### [Quickstart Guides](quickstart/)
Get up and running quickly with language-specific examples:
- [Python Quickstart](quickstart/python.md)
- [JavaScript Quickstart](quickstart/javascript.md)
- [Java Quickstart](quickstart/java.md)
- [C/C++ Quickstart](quickstart/c-cpp.md)
- [Go Quickstart](quickstart/go.md)
- [Haskell Quickstart](quickstart/haskell.md)

### [User Guide](guide/)
Learn how to use all features effectively:
- [Basic Usage](guide/basic-usage.md) - Core concepts and workflows
- [Advanced Features](guide/advanced-features.md) - Multiple variants, segmentation, in-place operations
- [Test Case Format](guide/test-cases.md) - Comprehensive test case documentation
- [Language Support](guide/languages.md) - Detailed language capabilities and requirements

- [Error Handling](guide/errors.md) - Understanding and resolving errors

### [Developer Documentation](developer/)
Extend and contribute to EiplGrader:
- [Architecture Overview](developer/architecture.md) - System design and components
- [Core Components](developer/components/) - Deep dive into CodeGenerator and CodeTester
- [Language System](developer/languages/) - Adding new languages and executors
- [API Reference](developer/api/) - Complete method documentation
- [Testing](developer/testing.md) - Test suite and quality assurance
- [Contributing](developer/contributing.md) - How to contribute


## 🔧 Installation Options

### Standard Installation
```bash
pip install eiplgrader
```

### Development Installation
```bash
git clone https://github.com/hamiltonfour/eiplgrader.git
cd eiplgrader
pip install -e ".[dev]"
```



## 🌟 Language Support at a Glance

| Language | Type System | Type Inference | Test Format |
|----------|-------------|----------------|-------------|
| Python | Dynamic | ✅ Automatic | Simplified |
| JavaScript | Dynamic | ✅ Automatic | Simplified |
| Go | Static | ❌ Required | Explicit |
| Java | Static | ❌ Required | Explicit |
| C++ | Static | ❌ Required | Explicit |
| C | Static | ❌ Required | Explicit |
| Haskell | Static | ❌ Required | Explicit |

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guide](developer/contributing.md) for details on:
- Setting up a development environment
- Running tests and linting
- Submitting pull requests
- Adding new languages or features

## 📖 Citation

If you use EiplGrader in your research or teaching, please cite:

```bibtex
@inproceedings{smith2024code,
    author = {Smith IV, David H. and Zilles, Craig},
    title = {Code Generation Based Grading: Evaluating an Auto-grading Mechanism 
             for "Explain-in-Plain-English" Questions},
    year = {2024},
    publisher = {Association for Computing Machinery},
    doi = {10.1145/3649217.3653582},
    booktitle = {Proceedings of the 2024 on Innovation and Technology 
                 in Computer Science Education V. 1},
    pages = {171–177}
}
```

## ⚠️ Important Notice

This is a research tool. Breaking changes between versions are expected. Always test thoroughly before using in production grading scenarios.

## 🔗 Quick Links

- [GitHub Repository](https://github.com/hamiltonfour/eiplgrader)
- [PyPI Package](https://pypi.org/project/eiplgrader/)

- [Issue Tracker](https://github.com/hamiltonfour/eiplgrader/issues)

---

Ready to get started? Check out our [Python Quickstart](quickstart/python.md) or browse the [full documentation](guide/basic-usage.md).
