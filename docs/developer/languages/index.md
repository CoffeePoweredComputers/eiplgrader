---
layout: default
title: Language System
parent: Developer Documentation
nav_order: 3
has_children: true
permalink: /developer/languages/
---

# Language System

Learn how EiplGrader's pluggable language support system works.

## Overview

The language system provides a flexible architecture for supporting multiple programming languages through a consistent interface. Each language is supported by two components:

1. **Language Adapter** - Handles code generation specifics
2. **Language Executor** - Handles code execution and testing

## System Architecture

![Language System Overview]({{ site.baseurl }}/assets/diagrams/index_diagram_1.svg)

## Documentation Sections

### 🏗️ [Architecture](architecture.html)
Understand the language system design.
- Component relationships
- Base class hierarchy
- Registry pattern
- Type system integration

### ➕ [Adding Languages](adding-languages.html)
Step-by-step guide to adding new language support.
- Creating adapters
- Creating executors
- Registration process
- Testing requirements

### ⚙️ [Executors](executors.html)
Deep dive into language executors.
- Interpreted vs compiled models
- Type inference vs validation
- Test harness generation
- Output normalization

## Key Concepts

### Language Adapter
Responsible for:
- Generating language-specific prompts
- Extracting code from LLM responses
- Normalizing code format
- Providing language configuration

### Language Executor
Responsible for:
- Preparing code with test harness
- Executing tests (interpret or compile)
- Handling type systems
- Processing results

### Language Registry
Central registration providing:
- Language discovery
- Component instantiation
- Configuration management
- Feature detection

## Type System Support

### Dynamic Languages (Type Inference)
- **Python**: Full automatic inference
- **JavaScript**: Full automatic inference

```python
# No type annotations needed
test_case = {
    "parameters": {"x": 5},
    "expected": 10
}
```

### Static Languages (Type Validation)
- **Java, C++, C, Go, Haskell**: Explicit types required

```python
# Type annotations required
test_case = {
    "parameters": {"x": 5},
    "parameter_types": {"x": "int"},
    "expected": 10,
    "expected_type": "int"
}
```

## Adding a New Language

Quick overview (see [full guide](adding-languages.html) for details):

1. **Create Adapter**
   ```python
   class NewLangAdapter(LanguageAdapter):
       def generate_prompt(self, ...):
           # Language-specific prompt
       def extract_code(self, ...):
           # Extract from LLM response
   ```

2. **Create Executor**
   ```python
   class NewLangExecutor(CompiledLanguageExecutor):
       def prepare_code(self, ...):
           # Add test harness
       def execute_test(self, ...):
           # Run the test
   ```

3. **Register Language**
   ```python
   registry.register(
       "newlang",
       NewLangAdapter,
       NewLangExecutor
   )
   ```

## Language Features

### Interpreted Languages
- Direct execution via interpreter
- Type inference support
- Faster development cycle
- Examples: Python, JavaScript

### Compiled Languages
- Compilation step required
- Type validation required
- Better performance
- Examples: Java, C++, Go

### Special Features by Language
- **Python**: AST-based normalization
- **JavaScript**: Async/Promise support
- **Java**: Solution class wrapping
- **Go**: Import management
- **C++**: STL container support
- **Haskell**: Lazy evaluation

## Testing Language Support

Each language implementation requires:
- Unit tests for adapter methods
- Unit tests for executor methods
- Integration tests with real code
- End-to-end tests with CodeGenerator/CodeTester

## Performance Considerations

### Executor Pooling
Reuse executors for better performance:
```python
executor_pool = ExecutorPool(max_size=10)
executor = executor_pool.acquire("python")
# Use executor
executor_pool.release("python", executor)
```

### Compilation Caching
Cache compiled binaries for static languages:
```python
if cache.has(code_hash):
    binary = cache.get(code_hash)
else:
    binary = compile(code)
    cache.put(code_hash, binary)
```

## Next Steps

- Understand the [Architecture](architecture.html) in detail
- Learn how to [Add New Languages](adding-languages.html)
- Explore [Executor](executors.html) implementation patterns
- See existing implementations in the source code