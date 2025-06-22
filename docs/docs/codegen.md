---
layout: default
title: Code Generation Module
nav_order: 2
---

# Code Generation Module

The `codegen.py` file contains the `CodeGenerator` class, which generates Python code using LLM APIs such as OpenAI's GPT models, with support for Anthropic and Meta models planned for future implementations.

## Classes

### `CodeGenerator`

This class uses language models to generate Python functions based on prompts, with support for multiple generation strategies.

#### Methods

- `__init__(api_key: str, client_type: str = "openai")`
  - Initializes the generator with an API key and the client type.
  - Currently supported client types: `"openai"` (with `"anthropic"` and `"meta"` planned for future releases)

- `generate_code(student_response: str, gen_type: str = "cgbg", params: str = "", assumptions: str = "", num_to_gen: int = 1, segmentation_few_shot_file: str = "", temperature: float = 1.0, model: str = "gpt-4o", function_name: str = "foo") -> Dict[str, Any]`
  - Generates code based on the provided parameters.
  - Parameters:
    - `student_response`: The student's prompt or function name (depends on gen_type)
    - `gen_type`: Generation type - `"cgbg"` for standard generation or `"redef"` for function redefinition
    - `params`: Function parameters (used with `"redef"` gen_type)
    - `assumptions`: Assumptions about the parameters (used with `"redef"` gen_type)
    - `num_to_gen`: Number of function variants to generate
    - `segmentation_few_shot_file`: Path to a JSON file with segmentation examples
    - `temperature`: Controls randomness in generation (0.0-1.0)
    - `model`: Model name to use (e.g., "gpt-4o")
    - `function_name`: Name to use for the generated function

## Generation Types

### Standard Generation (cgbg)

Generate a function that implements the given description.

```python
from eiplgrader.codegen import CodeGenerator

code_generator = CodeGenerator("YOUR_API_KEY")
result = code_generator.generate_code(
    "that adds two numbers.",
    function_name="add_numbers"
)
```

### Function Redefinition (redef)

Generate a function from a signature with optional parameter assumptions.

```python
from eiplgrader.codegen import CodeGenerator

code_generator = CodeGenerator("YOUR_API_KEY")
result = code_generator.generate_code(
    "sum_two_numbers",     # Function name to redefine
    gen_type="redef",
    params="a, b",         # Function parameters
    assumptions="a and b are integers"
)
```

## Multiple Generations

Generate multiple variations of a function:

```python
from eiplgrader.codegen import CodeGenerator

code_generator = CodeGenerator("YOUR_API_KEY")
result = code_generator.generate_code(
    "that adds two numbers.",
    num_to_gen=5           # Generate 5 variations
)
```

## Segmentation

Segment code to map explanation phrases to code segments using few-shot examples:

```python
from eiplgrader.codegen import CodeGenerator

code_generator = CodeGenerator("YOUR_API_KEY")
result = code_generator.generate_code(
    "iterates through a list of numbers and sums them up.",
    segmentation_few_shot_file="segmentation_few_shot.json"
)
```

The segmentation file should contain examples in a specific format, with "multistructural" and "relational" examples, each containing a "description" and "segmentation" mapping.

## Return Value

The `generate_code` method returns a dictionary with the following structure:

- Without segmentation: `{"code": [generated code string(s)]}`
- With segmentation: `{"code": [generated code string(s)], "segmentation": [segmentation results]}`

For multiple generated functions, the `code` field will contain a list of strings.
