---
layout: default
title: Core Components
parent: Developer Documentation
nav_order: 2
has_children: true
permalink: /developer/components/
---

# Core Components

Deep dive into the main components that power EiplGrader.

## Component Overview

EiplGrader consists of two primary components that work together to transform natural language descriptions into tested code:

### 🧠 [CodeGenerator](codegen.html)
Transforms natural language descriptions into executable code using Large Language Models.

**Key Features:**
- Multiple LLM provider support (OpenAI, Ollama - with Anthropic and Meta planned)
- Language-specific prompt engineering
- Multi-variant generation
- Code segmentation capabilities
- Temperature control for output variety

**Example:**
```python
generator = CodeGenerator(api_key, language="python")
result = generator.generate_code(
    student_response="calculates the factorial of a number",
    function_name="factorial",
    num_to_gen=3  # Generate 3 variants
)
```

### 🧪 [CodeTester](tester.html)
Executes generated code against predefined test cases and provides detailed results.

**Key Features:**
- Multi-language execution support
- Dynamic and static type handling
- In-place modification testing
- Parallel test execution
- Detailed error reporting

**Example:**
```python
tester = CodeTester(
    code=generated_code,
    test_cases=test_cases,
    function_name="factorial",
    language="python"
)
results = tester.run_tests()
```

## Component Interaction

![Component Interaction Sequence]({{ site.baseurl }}/assets/diagrams/components_corrected.svg)

## Design Patterns

### Factory Pattern
Both components use factory patterns for language-specific implementations:

```python
# CodeGenerator loads language adapter
adapter = LanguageRegistry.get_adapter(language)

# CodeTester loads language executor  
executor = LanguageRegistry.get_executor(language)
```

### Strategy Pattern
Different strategies for code generation and execution:

```python
# Generation strategies
if gen_type == "cgbg":
    prompt = adapter.generate_cgbg_prompt(...)
elif gen_type == "redef":
    prompt = adapter.generate_redef_prompt(...)

# Execution strategies
if isinstance(executor, InterpretedLanguageExecutor):
    result = executor.execute_with_interpreter(...)
else:
    result = executor.compile_and_execute(...)
```

### Template Method Pattern
Base classes define the algorithm structure:

```python
class LanguageExecutor(ABC):
    def execute_test(self, code, test_case):
        # Template method
        self.validate_input(test_case)
        prepared = self.prepare_code(code, test_case)
        result = self.run_code(prepared)
        return self.process_result(result)
```

## Extension Points

### Adding LLM Providers
1. Implement `ModelRequest` interface
2. Add to `CodeGenerator` client types
3. Handle provider-specific formatting

### Custom Test Runners
1. Extend `CodeTester` class
2. Override execution methods
3. Add custom result processing

### New Generation Types
1. Add prompt templates to adapters
2. Update `generate_prompt` methods
3. Document new generation type

## Best Practices

### Error Handling
- Always wrap external calls (LLM, subprocess)
- Provide meaningful error messages
- Distinguish between structural and runtime errors

### Resource Management
- Clean up temporary files
- Use context managers for resources
- Implement proper cleanup methods

### Performance
- Cache where appropriate

- Minimize redundant operations

## Component Testing

Each component has comprehensive test coverage:

- **Unit Tests**: Test individual methods
- **Integration Tests**: Test component interactions
- **End-to-End Tests**: Test complete workflows

See the testing documentation for each component for details.

## Next Steps

- Deep dive into [CodeGenerator](codegen.html) implementation
- Explore [CodeTester](tester.html) architecture
- Learn about the [Language System](../languages/) that supports both components