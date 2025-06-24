---
layout: default
title: CodeGenerator API
parent: API Reference
grand_parent: Developer Documentation
nav_order: 1
permalink: /developer/api/codegen-api
---

# CodeGenerator API Reference

Complete API documentation for the CodeGenerator class.

## Class: CodeGenerator

```python
from eiplgrader.codegen import CodeGenerator
```

### Constructor

```python
def __init__(
    self,
    api_key: str,
    client_type: str = "openai",
    ollama_base_url: str = "http://localhost:11434",
    language: str = "python"
)
```

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `api_key` | `str` | required | API key for the chosen model provider |
| `client_type` | `str` | `"openai"` | Model provider: "openai", "ollama" (Anthropic and Meta planned) |
| `ollama_base_url` | `str` | `"http://localhost:11434"` | Base URL for Ollama API |
| `language` | `str` | `"python"` | Target programming language |

#### Provider-Specific Options

**OpenAI:**
```python
generator = CodeGenerator(
    api_key=key,
    client_type="openai",
    language="python"
)
```

**Anthropic (Planned - Not Yet Implemented):**
```python
# Note: Anthropic support is planned for future releases
# The following is the expected API once implemented:
# generator = CodeGenerator(
#     api_key=key,
#     model="anthropic",
#     model_name="claude-3-opus-20240229",
#     max_tokens_to_sample=2048
# )
```

**Ollama:**
```python
generator = CodeGenerator(
    api_key="",  # Not needed for Ollama
    client_type="ollama",
    ollama_base_url="http://localhost:11434",
    language="python"
)
```

### Methods

#### generate_code

```python
def generate_code(
    self,
    student_response: str,
    gen_type: str = "cgbg",
    params: str = "",
    assumptions: str = "",
    num_to_gen: int = 1,
    segmentation_few_shot_file: str = "",
    temperature: float = 1.0,
    model: str = "gpt-4o",
    function_name: str = "foo",
    language: Optional[str] = None
) -> Dict[str, Any]
```

Generate code implementations from natural language descriptions.

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `student_response` | `str` | required | Natural language description or function name (for redef) |
| `gen_type` | `str` | `"cgbg"` | Generation type: "cgbg" or "redef" |
| `params` | `str` | `""` | Function parameters (for redef type) |
| `assumptions` | `str` | `""` | Parameter assumptions (for redef type) |
| `num_to_gen` | `int` | `1` | Number of implementations to generate |
| `segmentation_few_shot_file` | `str` | `""` | Path to segmentation examples file |
| `temperature` | `float` | `1.0` | LLM temperature (0.0-2.0) |
| `model` | `str` | `"gpt-4o"` | LLM model name |
| `function_name` | `str` | `"foo"` | Name for the generated function |
| `language` | `str` | `None` | Override instance language |

##### Returns

Dictionary containing:

```python
{
    "code": List[str],           # Generated code implementations
    "language": str,             # Language used for generation
    "segmentation": Optional[Dict]  # Segmentation results if requested
}
```

##### Example Usage

```python
# CGBG Generation
result = generator.generate_code(
    student_response="calculates the factorial of a number recursively",
    function_name="factorial",
    gen_type="cgbg",
    num_to_gen=3,
    temperature=0.7
)

# Redef Generation
result = generator.generate_code(
    student_response="factorial",  # Function name
    function_name="factorial",
    gen_type="redef",
    params="n",
    assumptions="n is non-negative integer"
)
```

### Configuration

#### Model-Specific Settings

```python
# OpenAI Configuration
generator = CodeGenerator(
    api_key=key,
    client_type="openai",
    language="python"
)

# Anthropic Configuration (Planned - Not Yet Implemented)
# generator = CodeGenerator(
#     api_key=key,
#     model="anthropic",
#     model_name="claude-3-opus-20240229",
#     temperature=0.7,
#     max_tokens_to_sample=2048,
#     stop_sequences=["\n\nHuman:", "\n\nAssistant:"]
# )

# Ollama Configuration
generator = CodeGenerator(
    api_key="",  # Not needed for Ollama
    client_type="ollama",
    ollama_base_url="http://localhost:11434",
    language="python"
)
```

### Advanced Usage

#### Batch Generation

```python
def generate_batch(responses: List[str], function_names: List[str]):
    """Generate code for multiple prompts efficiently."""
    results = []
    
    for response, func_name in zip(responses, function_names):
        try:
            result = generator.generate_code(
                student_response=response,
                function_name=func_name,
                num_to_gen=1
            )
            results.append(result)
        except GenerationError as e:
            results.append(None)
            print(f"Failed for {func_name}: {e}")
    
    return results
```

#### Custom Prompt Templates

```python
class CustomGenerator(CodeGenerator):
    def customize_prompt(self, base_prompt: str) -> str:
        """Add custom instructions to prompts."""
        custom_instructions = """
        Additional requirements:
        - Use descriptive variable names
        - Add error handling for edge cases
        - Include docstrings
        """
        return base_prompt + custom_instructions
```

#### Response Caching

```python
from functools import lru_cache

class CachedGenerator(CodeGenerator):
    @lru_cache(maxsize=100)
    def generate_code(self, *args, **kwargs):
        """Cache generation results."""
        # Convert kwargs to hashable form
        cache_key = str(args) + str(sorted(kwargs.items()))
        return super().generate_code(*args, **kwargs)
```

### Best Practices

1. **Error Handling**
   ```python
   try:
       result = generator.generate_code(...)
   except Exception as e:
       logging.error(f"Generation failed: {e}")
       # Implement retry logic
   ```

3. **Temperature Selection**
   - Use lower temperature (0.2-0.5) for deterministic output
   - Use higher temperature (0.7-1.0) for creative variations
   - Adjust based on generation type and requirements

4. **Prompt Engineering**
   - Provide clear, specific descriptions
   - Include examples for better results
   - Use appropriate generation type (cgbg vs redef)

## See Also

- [CodeTester API](tester-api.html) - For testing generated code
- [Language System API](language-api.html) - For language-specific details
- [Examples](examples.html) - Common usage patterns