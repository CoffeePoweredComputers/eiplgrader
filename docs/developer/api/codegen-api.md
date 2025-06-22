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
    model: str = "openai",
    language: str = "python",
    temperature: float = 0.7,
    max_tokens: int = 2048,
    rate_limiter: Optional[RateLimiter] = None,
    **kwargs
)
```

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `api_key` | `str` | required | API key for the chosen model provider |
| `model` | `str` | `"openai"` | Model provider: "openai", "anthropic", "ollama", "meta" |
| `language` | `str` | `"python"` | Target programming language |
| `temperature` | `float` | `0.7` | Sampling temperature (0.0-2.0) |
| `max_tokens` | `int` | `2048` | Maximum tokens in response |
| `rate_limiter` | `RateLimiter` | `None` | Optional rate limiting handler |
| `**kwargs` | `dict` | `{}` | Additional provider-specific options |

#### Provider-Specific Options

**OpenAI:**
```python
generator = CodeGenerator(
    api_key=key,
    model="openai",
    model_name="gpt-4",  # or "gpt-3.5-turbo"
    organization_id="org-xxx"  # Optional
)
```

**Anthropic:**
```python
generator = CodeGenerator(
    api_key=key,
    model="anthropic",
    model_name="claude-3-opus-20240229",
    max_tokens_to_sample=2048
)
```

**Ollama:**
```python
generator = CodeGenerator(
    api_key="",  # Not needed for Ollama
    model="ollama",
    model_name="codellama",
    base_url="http://localhost:11434"
)
```

### Methods

#### generate_code

```python
def generate_code(
    self,
    student_response: str,
    function_name: str,
    gen_type: str = "cgbg",
    num_to_gen: int = 1,
    temperature: Optional[float] = None,
    example_inputs: Optional[List] = None,
    example_outputs: Optional[List] = None,
    assumptions: Optional[str] = None,
    function_signature: Optional[str] = None,
    **kwargs
) -> GenerationResult
```

Generate code implementations from natural language descriptions.

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `student_response` | `str` | required | Natural language description of the function |
| `function_name` | `str` | required | Name for the generated function |
| `gen_type` | `str` | `"cgbg"` | Generation type: "cgbg" or "redef" |
| `num_to_gen` | `int` | `1` | Number of implementations to generate |
| `temperature` | `float` | `None` | Override instance temperature |
| `example_inputs` | `List` | `None` | Example input values (for cgbg) |
| `example_outputs` | `List` | `None` | Example output values (for cgbg) |
| `assumptions` | `str` | `None` | Additional assumptions (for redef) |
| `function_signature` | `str` | `None` | Function signature (for redef) |
| `**kwargs` | `dict` | `{}` | Additional language-specific options |

##### Returns

`GenerationResult` object containing:

```python
class GenerationResult:
    codes: List[str]              # Generated code implementations
    raw_response: str             # Raw LLM response
    prompt_used: str              # Actual prompt sent
    metadata: Dict[str, Any]      # Generation metadata
    success: bool                 # Whether generation succeeded
    error: Optional[str]          # Error message if failed
```

##### Example Usage

```python
# CGBG Generation
result = generator.generate_code(
    student_response="calculates the factorial of a number recursively",
    function_name="factorial",
    gen_type="cgbg",
    num_to_gen=3,
    example_inputs=[[5], [0], [10]],
    example_outputs=[120, 1, 3628800]
)

# Redef Generation
result = generator.generate_code(
    student_response="implements recursive factorial calculation",
    function_name="factorial",
    gen_type="redef",
    function_signature="def factorial(n: int) -> int:",
    assumptions="n is non-negative integer"
)
```

#### generate_with_segmentation

```python
def generate_with_segmentation(
    self,
    student_response: str,
    function_name: str,
    explanation_text: str,
    **kwargs
) -> SegmentedGenerationResult
```

Generate code with explanation-to-code segment mapping.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `student_response` | `str` | Natural language description |
| `function_name` | `str` | Function name |
| `explanation_text` | `str` | Detailed explanation to segment |
| `**kwargs` | `dict` | Additional generation parameters |

##### Returns

```python
class SegmentedGenerationResult(GenerationResult):
    segments: List[CodeSegment]   # Explanation-to-code mappings
    
class CodeSegment:
    explanation: str              # Part of explanation
    code_lines: List[int]        # Corresponding line numbers
    confidence: float            # Mapping confidence (0-1)
```

##### Example

```python
result = generator.generate_with_segmentation(
    student_response="sorts a list using quicksort",
    function_name="quicksort",
    explanation_text="""
    First, choose a pivot element from the list.
    Then partition the list around the pivot.
    Finally, recursively sort the sublists.
    """
)

for segment in result.segments:
    print(f"Explanation: {segment.explanation}")
    print(f"Code lines: {segment.code_lines}")
```

#### set_language

```python
def set_language(self, language: str) -> None
```

Change the target programming language.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `language` | `str` | Language name (e.g., "python", "java", "javascript") |

##### Raises

- `LanguageNotSupportedError`: If language is not supported

##### Example

```python
generator.set_language("java")
```

#### get_supported_languages

```python
@classmethod
def get_supported_languages(cls) -> List[str]
```

Get list of supported programming languages.

##### Returns

List of language identifiers.

##### Example

```python
languages = CodeGenerator.get_supported_languages()
# ['python', 'javascript', 'java', 'cpp', 'c', 'go', 'haskell']
```

#### estimate_tokens

```python
def estimate_tokens(
    self,
    prompt: str,
    model: Optional[str] = None
) -> int
```

Estimate token count for a prompt.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `prompt` | `str` | Text to estimate |
| `model` | `str` | Optional model override |

##### Returns

Estimated token count.

##### Example

```python
tokens = generator.estimate_tokens(prompt)
if tokens > 4000:
    print("Warning: Prompt may be too long")
```

### Properties

#### language

```python
@property
def language(self) -> str
```

Get current target language.

#### model_info

```python
@property
def model_info(self) -> Dict[str, Any]
```

Get model configuration information.

```python
info = generator.model_info
# {
#     'provider': 'openai',
#     'model_name': 'gpt-4',
#     'temperature': 0.7,
#     'max_tokens': 2048
# }
```

### Exceptions

#### GenerationError

```python
class GenerationError(EiplGraderError):
    """Raised when code generation fails."""
    
    def __init__(self, message: str, details: Dict[str, Any] = None):
        self.details = details or {}
        super().__init__(message)
```

Common causes:
- API key invalid
- Rate limit exceeded
- Model timeout
- Invalid prompt format

#### LanguageNotSupportedError

```python
class LanguageNotSupportedError(GenerationError):
    """Raised when requested language is not supported."""
    
    def __init__(self, language: str, supported: List[str]):
        self.language = language
        self.supported = supported
        super().__init__(
            f"Language '{language}' not supported. "
            f"Supported: {', '.join(supported)}"
        )
```

### Configuration

#### Model-Specific Settings

```python
# OpenAI Configuration
generator = CodeGenerator(
    api_key=key,
    model="openai",
    model_name="gpt-4",
    temperature=0.7,
    top_p=0.95,
    frequency_penalty=0.0,
    presence_penalty=0.0,
    stop_sequences=["\n\n", "```"]
)

# Anthropic Configuration
generator = CodeGenerator(
    api_key=key,
    model="anthropic",
    model_name="claude-3-opus-20240229",
    temperature=0.7,
    max_tokens_to_sample=2048,
    stop_sequences=["\n\nHuman:", "\n\nAssistant:"]
)

# Ollama Configuration
generator = CodeGenerator(
    api_key="",
    model="ollama",
    model_name="codellama",
    base_url="http://localhost:11434",
    temperature=0.7,
    num_predict=2048,
    top_k=40,
    top_p=0.9
)
```

#### Language-Specific Options

```python
# Java with specific version
result = generator.generate_code(
    student_response="implement quicksort",
    function_name="quickSort",
    language_version="11",  # Java 11
    use_generics=True
)

# Python with type hints
result = generator.generate_code(
    student_response="calculate fibonacci",
    function_name="fibonacci",
    use_type_hints=True,
    python_version="3.9"
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
   except GenerationError as e:
       logging.error(f"Generation failed: {e}")
       # Implement retry logic
   ```

2. **Rate Limiting**
   ```python
   rate_limiter = RateLimiter(
       max_requests_per_minute=60
   )
   generator = CodeGenerator(
       api_key=key,
       rate_limiter=rate_limiter
   )
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