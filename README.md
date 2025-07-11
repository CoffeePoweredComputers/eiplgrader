
# EiplGrader

<div align="center">
    <img src="./eipllogo.png" alt="Explain in Plain Language Autograder" style="width: 300px; border-radius: 8px; margin-bottom: 20px;">
    <h3>An automatic grading suite for "Explain in Plain Language" questions</h3>

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

**⚠️ Research Tool Notice:** This is a research tool. Breaking changes between versions are expected.

EiplGrader automatically grades programming assignments where students explain algorithms in natural language. It uses large language models to generate code from student explanations and tests that code against predefined test cases.

## Quick Start

```bash
pip install eiplgrader
```

Set up your API keys:

**Option 1: Environment Variables**
```bash
export OPENAI_API_KEY="your-api-key-here"    # For OpenAI models
export META_API_KEY="your-api-key-here"      # For Meta/Llama models
# export ANTHROPIC_API_KEY="your-api-key-here" # Coming soon
```

**Option 2: Using .env file**
```bash
cp .env.example .env
# Edit .env and add your API keys
```

### Example: Code Generation Based Grading (CGBG)

```python
from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester
import os

# Choose your provider and get the appropriate API key
client_type = "openai"  # or "meta" for Llama, "ollama" for local models
api_key = os.getenv("OPENAI_API_KEY")  # or META_API_KEY for Llama

# Generate code from natural language description
code_generator = CodeGenerator(api_key, client_type=client_type, language="python")
result = code_generator.generate_code(
    student_response="that adds two numbers and returns the result",
    model="gpt-4o",  # or "Llama-4-Maverick-17B-128E-Instruct-FP8" for Llama
    function_name="add_numbers",
    gen_type="cgbg"
)

# Test the generated code
test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 3},
    {"parameters": {"a": -1, "b": 1}, "expected": 0},
    {"parameters": {"a": 0, "b": 0}, "expected": 0}
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

## Features

- **Multi-language Support**: Python, JavaScript, Java, C++, C, Go, Haskell
- **LLM Providers**: OpenAI (GPT models), Meta (Llama models), Ollama (local models)
- **Type Inference**: Automatic type detection for Python and JavaScript
- **Multiple Generation Modes**: CGBG (Code Generation), Function Redefinition, Segmentation
- **Comprehensive Testing**: Built-in test runner with detailed results
- **Research-backed**: Based on peer-reviewed educational research

## Resources

- **📚 [Full Documentation](https://hamiltonfour.tech/eiplgrader/)**
- **🔧 [Complete Examples](./examples/)** - See `example_cgbg.py`, `example_redef.py`, `example_segmentation.py`
- **📝 [Test Cases Format Guide](./examples/example_test_cases_python.json)**

## Language Support

| Language   | Type Inference | Type Annotations Required |
|------------|----------------|---------------------------|
| Python     | ✅ Automatic   | Optional                  |
| JavaScript | ✅ Automatic   | Optional                  |
| Java       | ❌             | Required                  |
| C++        | ❌             | Required                  |
| C          | ❌             | Required                  |
| Go         | ❌             | Required                  |
| Haskell    | ❌             | Required                  |

## Supported LLM Providers

| Provider | Models | Status | Environment Variable |
|----------|--------|--------|--------------------|
| OpenAI | GPT-4o, GPT-4, GPT-4.5, GPT-4.1 | ✅ Fully Supported | `OPENAI_API_KEY` |
| Meta | Llama-4-Maverick-17B-128E-Instruct-FP8, Llama-4-Scout-17B-16E-Instruct-FP8, Llama-3.3-70B-Instruct, Llama-3.3-8B-Instruct | ✅ Fully Supported | `META_API_KEY` |
| Ollama | codellama:instruct, stable-code:instruct | ✅ Fully Supported | N/A (local) |
| Anthropic | Claude models | 🚧 Coming Soon | `ANTHROPIC_API_KEY` |

## Planned Features

The following features are planned for future releases:

- **Additional LLM Providers**: Anthropic (Claude) support
- **Enhanced Segmentation**: Improved mapping between explanations and code
- **Performance Optimizations**: Faster test execution and parallel processing
- **Extended Language Support**: Rust, TypeScript, and more

## Citation

When using this tool, please cite:

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

## Related Research

- [Code Generation Based Grading: Evaluating an Auto-grading Mechanism for "Explain-in-Plain-English" Questions](https://doi.org/10.1145/3649217.3653582)
- [Explaining Code with a Purpose: An Integrated Approach for Developing Code Comprehension and Prompting Skills](https://doi.org/10.1145/3649217.3653587)
- [Prompting for Comprehension: Exploring the Intersection of Explain in Plain English Questions and Prompt Writing](https://doi.org/10.1145/3657604.3662039)
