---
layout: default
title: Code Generation Module
nav_order: 2
---

# Code Generation Module

The `codegen.py` file contains the `CodeGenerator` class, which generates Python code using OpenAI's GPT models.

## Classes

### `CodeGenerator`

This class uses OpenAI's GPT model to generate Python functions based on a prompt.

#### Methods

- `__init__(api_key: str, model: str = "gpt-4o", temperature: float = 0, system_prompt: str = SYSTEM_PROMPT)`
  - Initializes the generator with an API key and other parameters for generating code.
  
- `generate_code(student_response: str)`
  - Takes the student's response and generates the code based on the prompt.

Example usage:

```python
code_generator = CodeGenerator("YOUR API KEY HERE")
generated_code = code_generator.generate_code("that adds two numbers.")
```
