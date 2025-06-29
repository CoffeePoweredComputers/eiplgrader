---
layout: default
title: Go Quickstart
parent: Quickstart Guides
nav_order: 5
---

# Go Quickstart

Get up and running with EiplGrader for Go in minutes.

## Prerequisites

- Go 1.16+
- Python 3.7+ (for running EiplGrader)
- API key for your chosen provider (OpenAI, Meta/Llama, or Ollama local models)

## Installation

```bash
pip install eiplgrader
```

## Basic Example

### 1. Generate Go Code

```python
import os
from eiplgrader.codegen import CodeGenerator

# Initialize the code generator for Go
# Choose your provider: "openai", "meta", "ollama"
client_type = "openai"  # or "meta" for Llama
api_key = os.getenv("OPENAI_API_KEY")  # or META_API_KEY for Meta

generator = CodeGenerator(api_key, client_type=client_type, language="go")

# Generate code from# Generate code from a student's explanation
result = generator.generate_code(
    student_response="that filters even numbers from a slice of integers",
    function_name="filterEven",
    gen_type="cgbg"
)
result = generator.generate_code(
    student_response="that filters even numbers from a slice of integers",
    model="gpt-4o",  # or "Llama-4-Maverick-17B-128E-Instruct-FP8" for Meta
    function_name="filterEven",
    gen_type="cgbg"
)

print("Generated code:")
print(result["code"][0])
```

Output:
```go
func filterEven(numbers []int) []int {
    var result []int
    for _, num := range numbers {
        if num%2 == 0 {
            result = append(result, num)
        }
    }
    return result
}
```

### 2. Test the Generated Code

```python
from eiplgrader.tester import CodeTester

# Define test cases - Go REQUIRES explicit type annotations
test_cases = [
    {
        "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
        "parameter_types": {"numbers": "[]int"},  # Go slice type
        "expected": [2, 4, 6],
        "expected_type": "[]int"
    },
    {
        "parameters": {"numbers": [1, 3, 5, 7]},
        "parameter_types": {"numbers": "[]int"},
        "expected": [],
        "expected_type": "[]int"
    },
    {
        "parameters": {"numbers": []},
        "parameter_types": {"numbers": "[]int"},
        "expected": [],
        "expected_type": "[]int"
    }
]

# Create and run the tester
tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name="filterEven",
    language="go"
)

results = tester.run_tests()
print(f"Tests passed: {results.successes}/{results.testsRun}")
```

## Go-Specific Features

### Type Annotations are REQUIRED

Despite Go having type inference in the language, EiplGrader requires explicit types:

```python
# CORRECT - with type annotations
test_case = {
    "parameters": {"name": "Alice", "age": 25},
    "parameter_types": {"name": "string", "age": "int"},  # Required!
    "expected": "Alice is 25 years old",
    "expected_type": "string"  # Required!
}

# WRONG - missing type annotations
test_case = {
    "parameters": {"name": "Alice", "age": 25},
    "expected": "Alice is 25 years old"
    # This will fail with: "Missing required type information"
}
```

### Go Type Mappings

| Go Type | Example |
|---------|---------|
| `int` | `42` |
| `float64` | `3.14` |
| `string` | `"hello"` |
| `bool` | `true` |
| `[]int` | `[]int{1, 2, 3}` |
| `[]string` | `[]string{"a", "b"}` |

### Automatic Import Management

The test harness automatically manages imports based on usage:

```go
// These imports are added automatically when needed:
import "fmt"       // For basic output
import "encoding/json"  // For complex type output
import "os"        // For error handling
```


## Multiple Return Values

Go supports multiple return values, but test cases check the first return value:

```python
result = generator.generate_code(
    student_response="that divides two numbers and returns quotient and remainder",
    model="gpt-4o",  # or your chosen model
    function_name="divmod"
)

# Test harness only checks the first return value
test_cases = [
    {
        "parameters": {"a": 17, "b": 5},
        "parameter_types": {"a": "int", "b": "int"},
        "expected": 3,  # Only checking quotient
        "expected_type": "int"
    }
]
```

## Error Handling Patterns

Go's error handling is unique, but the test harness focuses on the primary return value:

```python
# Generate code that might return an error
result = generator.generate_code(
    student_response="that parses an integer from a string",
    model="gpt-4o",  # or your chosen model
    function_name="parseInteger"
)

# Test cases focus on successful cases
test_cases = [
    {
        "parameters": {"str": "123"},
        "parameter_types": {"str": "string"},
        "expected": 123,
        "expected_type": "int"
    }
]
```

## In-Place Modifications

Go supports in-place modifications through slices:

```python
# Mode 1: Modifies slice in-place
test_case = {
    "parameters": {"nums": [3, 1, 4, 1, 5]},
    "parameter_types": {"nums": "[]int"},
    "expected": [1, 1, 3, 4, 5],
    "expected_type": "[]int",
    "inplace": "1"  # Tests that nums is sorted in-place
}
```

## Advanced Features

### Type Conversions
```python
result = generator.generate_code(
    student_response="that converts a slice of strings to integers",
    model="gpt-4o",  # or your chosen model
    function_name="stringsToInts"
)

test_cases = [
    {
        "parameters": {"strings": ["1", "2", "3"]},
        "parameter_types": {"strings": "[]string"},
        "expected": [1, 2, 3],
        "expected_type": "[]int"
    }
]
```

### Working with Runes
```python
result = generator.generate_code(
    student_response="that counts unicode characters in a string",
    model="gpt-4o",  # or your chosen model
    function_name="countRunes"
)

test_cases = [
    {
        "parameters": {"text": "Hello 世界"},
        "parameter_types": {"text": "string"},
        "expected": 8,
        "expected_type": "int"
    }
]
```

## Output Format

Go uses different output formats based on the return type:

- **Simple types** (int, string, bool, float64): Direct fmt.Println
- **Complex types** (slices, maps): JSON encoding

```go
// Simple type output
fmt.Println(result)  // For int, string, etc.

// Complex type output
output, _ := json.Marshal(result)
os.Stdout.Write(output)  // For slices, maps
```

## Best Practices

1. **Always specify Go types**:
   ```python
   # Use Go-specific type names
   "parameter_types": {
       "nums": "[]int",      # not "List[int]"
       "value": "float64",   # not "double"
       "data": "[]string"    # not "List[string]"
   }
   ```

2. **Use idiomatic Go patterns**:
   ```go
   // Generated code should use:
   // - range loops
   // - append for slices
   // - make for maps
   // - proper error handling
   ```

3. **Be aware of zero values**:
   ```python
   # Go has zero values for all types
   test_cases = [
       {
           "parameters": {"count": 0},  # Zero value for int
           "parameter_types": {"count": "int"},
           "expected": 0,
           "expected_type": "int"
       }
   ]
   ```

4. **Slice vs Array**:
   ```python
   # Always use slices for flexibility
   "[]int"   # Slice (preferred)
   "[5]int"  # Array (fixed size, rarely needed)
   ```

## Common Gotchas

1. **nil slices vs empty slices**:
   ```python
   # Both are handled correctly
   "expected": []  # Can match both nil and empty slices
   ```

2. **Map ordering**:
   ```python
   # Maps are unordered, but JSON output is consistent
   "expected": {"a": 1, "b": 2}  # Order doesn't matter
   ```

3. **String immutability**:
   ```go
   // Strings are immutable in Go
   // Generated code should return new strings
   ```

4. **Interface types**:
   ```python
   # Avoid interface{} in test cases
   # Use concrete types instead
   ```

## Compilation and Execution

- **Compilation**: `go build -o output input.go`
- **Execution**: Binary with embedded test values
- **Package**: Always uses `package main`

## Error Messages

Go provides clear compilation and runtime errors:

```python
try:
    results = tester.run_tests()
    if not results.was_successful():
        for result in results.test_results:
            if not result["pass"]:
                print(f"Test failed: {result['function_call']}")
            # Common Go errors:
            # - "undefined: functionName"
            # - "cannot use x (type T) as type U"
            # - "index out of range"
except Exception as e:
    print(f"Error: {e}")
```

## Next Steps

- Compare with [Java Quickstart](java.md) for static typing similarities
- Explore [Haskell Quickstart](haskell.md) for functional programming
- Learn about [Test Case Format](../guide/test-cases.md) for complex types
- See [Language Support](../guide/languages.md) for Go-specific details