---
layout: default
title: CodeGenerator
parent: Core Components
grand_parent: Developer Documentation
nav_order: 1
---

# CodeGenerator Component

Deep dive into the CodeGenerator class implementation and extension.

## Overview

The `CodeGenerator` class is responsible for transforming natural language descriptions into executable code using Large Language Models (LLMs).

## Class Structure

```python
class CodeGenerator:
    """Generate code from natural language using LLM APIs."""
    
    def __init__(
        self,
        api_key: str,
        client_type: str = "openai",
        language: str = "python"
    ):
        """Initialize the code generator with API credentials."""
        self.api_key = api_key
        self.client_type = client_type
        self.language = language
        self._initialize_client()
        self._load_language_adapter()
```

## Key Methods

### `generate_code()`

The main method for code generation:

```python
def generate_code(
    self,
    student_response: str,
    function_name: str = "foo",
    gen_type: str = "cgbg",
    params: str = "",
    assumptions: str = "",
    num_to_gen: int = 1,
    segmentation_few_shot_file: str = "",
    temperature: float = 1.0,
    model: str = "gpt-4o"
) -> Dict[str, Any]:
    """
    Generate code from natural language description.
    
    Args:
        student_response: Natural language description or function name
        function_name: Name for the generated function
        gen_type: Generation type ('cgbg' or 'redef')
        params: Function parameters (for 'redef' type)
        assumptions: Parameter assumptions (for 'redef' type)
        num_to_gen: Number of variants to generate
        segmentation_few_shot_file: Path to segmentation examples
        temperature: LLM temperature (0.0-2.0)
        model: LLM model name
        
    Returns:
        Dictionary with 'code' list and optional 'segmentation'
    """
```

### Internal Methods

#### `_initialize_client()`

Sets up the LLM client based on provider:

```python
def _initialize_client(self):
    """Initialize the appropriate LLM client."""
    if self.client_type == "openai":
        self.client = OpenAIClient(self.api_key)
    elif self.client_type == "anthropic":
        # Placeholder - not yet implemented
        raise NotImplementedError("Anthropic support planned for future release")
    elif self.client_type == "meta":
        # Placeholder - not yet implemented
        raise NotImplementedError("Meta support planned for future release")
    elif self.client_type == "ollama":
        self.client = OllamaClient()
    else:
        raise ValueError(f"Unsupported client type: {self.client_type}")
```

#### `_load_language_adapter()`

Loads the language-specific adapter:

```python
def _load_language_adapter(self):
    """Load the appropriate language adapter."""
    from eiplgrader.languages.registry import LanguageRegistry
    
    registry = LanguageRegistry()
    self.adapter = registry.get_adapter(self.language)
```

## Prompt Engineering

### CGBG (Code Generation Based Grading)

The default generation mode creates prompts like:

```python
def _create_cgbg_prompt(self, student_response: str, function_name: str) -> str:
    """Create prompt for CGBG generation."""
    return self.adapter.generate_prompt(
        student_response=student_response,
        function_name=function_name,
        gen_type="cgbg",
        num_to_gen=self.num_to_gen
    )
```

Example prompt structure:
```
You are a Python programming student. Write a function named 'calculate_average'
that takes a list of numbers and returns their average.

Requirements:
- Function must be named exactly 'calculate_average'
- Must handle empty lists appropriately
- Return type should be float

Provide only the function implementation without explanations.
```

### Redef (Function Redefinition)

For implementing predefined interfaces:

```python
def _create_redef_prompt(
    self, 
    function_name: str, 
    params: str, 
    assumptions: str
) -> str:
    """Create prompt for function redefinition."""
    return self.adapter.generate_prompt(
        student_response=function_name,
        function_name=function_name,
        gen_type="redef",
        params=params,
        assumptions=assumptions
    )
```

Example prompt structure:
```
Implement the following function:

def process_data(items, threshold):
    """
    Assumptions:
    - items is a list of dictionaries with 'value' keys
    - threshold is a positive number
    """
    # Your implementation here
```

## Multiple Variant Generation

Generate multiple implementations:

```python
def _generate_variants(self, prompt: str, num_variants: int) -> List[str]:
    """Generate multiple code variants."""
    variants = []
    
    for i in range(num_variants):
        # Adjust temperature for variety
        temp = 0.7 + (i * 0.2)  # 0.7, 0.9, 1.1, etc.
        
        response = self.client.generate(
            prompt=prompt,
            temperature=min(temp, 1.5),
            max_tokens=1000
        )
        
        code = self.adapter.extract_code(response)
        if code:
            variants.extend(code)
    
    return variants[:num_variants]  # Limit to requested number
```

## Code Segmentation

Map natural language explanations to code segments:

```python
def _perform_segmentation(
    self, 
    code: str, 
    explanation: str, 
    few_shot_file: str
) -> List[Dict[str, Any]]:
    """Map explanation segments to code lines."""
    
    # Load few-shot examples
    with open(few_shot_file, 'r') as f:
        examples = json.load(f)
    
    # Create segmentation prompt
    prompt = self._create_segmentation_prompt(
        code=code,
        explanation=explanation,
        examples=examples
    )
    
    # Get segmentation from LLM
    response = self.client.generate(prompt)
    
    # Parse response into segment mappings
    return self._parse_segmentation_response(response)
```

## LLM Provider Integration

### OpenAI Provider

```python
class OpenAIProvider(ModelRequest):
    """OpenAI API integration."""
    
    def __init__(self, api_key: str):
        self.client = openai.Client(api_key=api_key)
    
    def request_function_generation(
        self,
        prompt: str,
        model: str = "gpt-4o",
        temperature: float = 1.0,
        max_tokens: int = 1000
    ) -> str:
        """Generate code using OpenAI API."""
        response = self.client.chat.completions.create(
            model=model,
            messages=[
                {"role": "system", "content": "You are a helpful coding assistant."},
                {"role": "user", "content": prompt}
            ],
            temperature=temperature,
            max_tokens=max_tokens
        )
        return response.choices[0].message.content
```

### Adding New Providers

To add a new LLM provider:

```python
class CustomProvider(ModelRequest):
    """Custom LLM provider integration."""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self._setup_client()
    
    def request_function_generation(self, prompt: str, **kwargs) -> str:
        """Implement code generation using custom API."""
        # Your implementation here
        pass
    
    def request_segmentation(self, prompt: str, **kwargs) -> str:
        """Implement segmentation using custom API."""
        # Your implementation here
        pass
```

## Error Handling

### API Errors

```python
def _handle_api_error(self, error: Exception) -> None:
    """Handle LLM API errors gracefully."""
    if isinstance(error, openai.RateLimitError):
        raise GenerationError(
            "Rate limit exceeded. Please try again later.",
            retry_after=error.retry_after
        )
    elif isinstance(error, openai.APIError):
        raise GenerationError(f"API error: {str(error)}")
    else:
        raise GenerationError(f"Unexpected error: {str(error)}")
```

### Validation Errors

```python
def _validate_generation_params(self, **kwargs) -> None:
    """Validate generation parameters."""
    gen_type = kwargs.get('gen_type', 'cgbg')
    
    if gen_type not in ['cgbg', 'redef']:
        raise ValueError(f"Invalid generation type: {gen_type}")
    
    if gen_type == 'redef':
        if not kwargs.get('params'):
            raise ValueError("Parameters required for 'redef' generation")
```

## Response Processing

### Code Extraction

```python
def _extract_code_blocks(self, llm_response: str) -> List[str]:
    """Extract code blocks from LLM response."""
    # Delegate to language adapter
    code_blocks = self.adapter.extract_code(llm_response)
    
    # Validate extracted code
    validated_blocks = []
    for code in code_blocks:
        if self._is_valid_code(code):
            validated_blocks.append(code)
    
    return validated_blocks
```

### Code Normalization

```python
def _normalize_code(self, code: str) -> str:
    """Normalize code format."""
    # Delegate to language adapter
    normalized = self.adapter.normalize_code(code)
    
    # Additional normalization if needed
    normalized = self._ensure_function_name(normalized, self.function_name)
    
    return normalized
```

## Caching Strategy

Implement caching for repeated requests:

```python
class CachedCodeGenerator(CodeGenerator):
    """Code generator with caching support."""
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.cache = {}
    
    def generate_code(self, student_response: str, **kwargs) -> Dict[str, Any]:
        """Generate code with caching."""
        # Create cache key
        cache_key = self._create_cache_key(student_response, **kwargs)
        
        # Check cache
        if cache_key in self.cache:
            return self.cache[cache_key]
        
        # Generate if not cached
        result = super().generate_code(student_response, **kwargs)
        
        # Cache result
        self.cache[cache_key] = result
        
        return result
```

## Testing the Generator

### Unit Tests

```python
class TestCodeGenerator(unittest.TestCase):
    """Test cases for CodeGenerator."""
    
    def setUp(self):
        self.generator = CodeGenerator("test_key", language="python")
    
    def test_cgbg_generation(self):
        """Test basic CGBG generation."""
        result = self.generator.generate_code(
            "adds two numbers",
            function_name="add"
        )
        
        self.assertIn("code", result)
        self.assertIsInstance(result["code"], list)
        self.assertTrue(len(result["code"]) > 0)
    
    def test_redef_generation(self):
        """Test function redefinition."""
        result = self.generator.generate_code(
            "multiply",
            gen_type="redef",
            params="x, y",
            assumptions="x and y are numbers"
        )
        
        self.assertIn("def multiply(x, y):", result["code"][0])
```

### Integration Tests

```python
def test_end_to_end_generation():
    """Test complete generation and execution flow."""
    # Generate code
    generator = CodeGenerator(api_key, language="python")
    result = generator.generate_code("calculates factorial")
    
    # Test generated code
    tester = CodeTester(
        code=result["code"][0],
        test_cases=[
            {"parameters": {"n": 5}, "expected": 120},
            {"parameters": {"n": 0}, "expected": 1}
        ],
        function_name="factorial",
        language="python"
    )
    
    test_results = tester.run_tests()
    assert test_results.was_successful()
```

## Performance Optimization

### Batch Generation

```python
def generate_batch(
    self, 
    prompts: List[str], 
    **kwargs
) -> List[Dict[str, Any]]:
    """Generate code for multiple prompts efficiently."""
    results = []
    
    # Group similar prompts
    grouped = self._group_similar_prompts(prompts)
    
    for group in grouped:
        # Generate with optimal parameters for group
        group_results = self._generate_group(group, **kwargs)
        results.extend(group_results)
    
    return results
```

### Streaming Support

```python
def generate_stream(
    self, 
    student_response: str, 
    **kwargs
) -> Iterator[str]:
    """Stream code generation for real-time feedback."""
    prompt = self._create_prompt(student_response, **kwargs)
    
    for chunk in self.client.stream_generate(prompt):
        # Process chunk and yield partial results
        if code_chunk := self._extract_partial_code(chunk):
            yield code_chunk
```

## Configuration

### Environment Variables

```python
# Default configuration from environment
DEFAULT_CONFIG = {
    "api_key": "your-api-key",
    "model": "gpt-4o",
    "temperature": 1.0
}
```

### Configuration File

```json
{
    "generation": {
        "default_model": "gpt-4o",
        "temperature_range": [0.0, 1.5],
        "max_tokens": 2000,
        "timeout": 30
    },
    "languages": {
        "python": {
            "model": "gpt-4o",
            "temperature": 0.8
        },
        "java": {
            "model": "gpt-4o",
            "temperature": 0.7
        }
    }
}
```

## Next Steps

- Explore [CodeTester Component](tester.md) for testing generated code
- Learn about [Language Adapters](../languages/architecture.md)
- See [API Reference](../api/) for complete method documentation
- Review [Examples](../../quickstart/) for practical usage