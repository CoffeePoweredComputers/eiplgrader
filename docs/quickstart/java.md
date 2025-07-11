---
layout: default
title: Java Quickstart
parent: Quickstart Guides
nav_order: 3
---

# Java Quickstart

Get up and running with EiplGrader for Java in minutes.

## Prerequisites

- Java JDK 8+
- Python 3.7+ (for running EiplGrader)
- API key for your chosen provider (OpenAI, Meta/Llama, or Ollama local models)

## Installation

```bash
pip install eiplgrader
```

## Basic Example

### 1. Generate Java Code

```python
import os
from eiplgrader.codegen import CodeGenerator

# Initialize the code generator for Java
# Choose your provider: "openai", "meta", "ollama"
client_type = "openai"  # or "meta" for Llama
api_key = os.getenv("OPENAI_API_KEY")  # or META_API_KEY for Meta

generator = CodeGenerator(api_key, client_type=client_type, language="java")

# Generate code from a student's explanation
result = generator.generate_code(
    student_response="that calculates the average of an array of integers",
    model="gpt-4o",  # or "Llama-4-Maverick-17B-128E-Instruct-FP8" for Meta
    function_name="calculateAverage",
    gen_type="cgbg"
)

print("Generated code:")
print(result["code"][0])
```

Output:
```java
public static double calculateAverage(int[] numbers) {
    if (numbers.length == 0) return 0;
    int sum = 0;
    for (int num : numbers) {
        sum += num;
    }
    return (double) sum / numbers.length;
}
```

### 2. Test the Generated Code

```python
from eiplgrader.tester import CodeTester

# Define test cases - Java REQUIRES explicit type annotations
test_cases = [
    {
        "parameters": {"numbers": [10, 20, 30, 40, 50]},
        "parameter_types": {"numbers": "int[]"},  # Required!
        "expected": 30.0,
        "expected_type": "double"  # Required!
    },
    {
        "parameters": {"numbers": [5, 5, 5]},
        "parameter_types": {"numbers": "int[]"},
        "expected": 5.0,
        "expected_type": "double"
    },
    {
        "parameters": {"numbers": []},
        "parameter_types": {"numbers": "int[]"},
        "expected": 0.0,
        "expected_type": "double"
    }
]

# Create and run the tester
tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name="calculateAverage",
    language="java"
)

results = tester.run_tests()
print(f"Tests passed: {results.successes}/{results.testsRun}")
```

## Java-Specific Features

### Type Annotations are REQUIRED

Unlike Python and JavaScript, Java requires explicit type information:

```python
# CORRECT - with type annotations
test_case = {
    "parameters": {"name": "Alice", "age": 25},
    "parameter_types": {"name": "String", "age": "int"},  # Required!
    "expected": "Alice is 25 years old",
    "expected_type": "String"  # Required!
}

# WRONG - missing type annotations
test_case = {
    "parameters": {"name": "Alice", "age": 25},
    "expected": "Alice is 25 years old"
    # This will fail with: "Missing required type information"
}
```

### Java Type Mappings

| Java Type | Example |
|-----------|---------|
| `int` | `42` |
| `double` | `3.14` |
| `String` | `"hello"` (Note: capital S) |
| `boolean` | `true` |
| `int[]` | `[1, 2, 3]` |
| `String[]` | `["a", "b"]` |

### Solution Class Wrapping

Generated code is automatically wrapped in a Solution class:

```python
# Your generated function:
result = generator.generate_code(
    student_response="that reverses a string",
    model="gpt-4o",  # or your chosen model
    function_name="reverseString"
)

# Becomes:
# class Solution {
#     public static String reverseString(String str) {
#         return new StringBuilder(str).reverse().toString();
#     }
# }
```


## In-Place Modifications

Java supports in-place modifications through array/object mutations:

```python
# Mode 1: Modifies array in-place
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5]},
    "parameter_types": {"arr": "int[]"},
    "expected": [1, 1, 3, 4, 5],
    "expected_type": "int[]",
    "inplace": "1"  # Tests that arr is sorted in-place
}
```

## Advanced Features

### Multiple Parameter Types
```python
result = generator.generate_code(
    student_response="that creates a summary from different data types",
    model="gpt-4o",  # or your chosen model
    function_name="createSummary"
)

test_cases = [
    {
        "parameters": {
            "title": "Report",
            "values": [10, 20, 30],
            "isPublic": True,
            "rating": 4.5
        },
        "parameter_types": {
            "title": "String",
            "values": "int[]",
            "isPublic": "boolean",
            "rating": "double"
        },
        "expected": "Report: 3 items, average 20.0, rating 4.5 (public)",
        "expected_type": "String"
    }
]
```

### Working with 2D Arrays
```python
result = generator.generate_code(
    student_response="that calculates the sum of a 2D matrix",
    function_name=",
    model="gpt-4o",  # or your chosen model
    function_name="matrixSum"
)

test_cases = [
    {
        "parameters": {
            "matrix": [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        },
        "parameter_types": {
            "matrix": "int[][]"  # 2D array type
        },
        "expected": 45,
        "expected_type": "int"
    }
]
```

## Error Handling

Java provides detailed compilation and runtime error information:

```python
try:
    results = tester.run_tests()
    if not results.was_successful():
        for result in results.test_results:
            if not result["pass"]:
                print(f"Test failed: {result['function_call']}")
                print(f"Expected: {result['expected_output']}")
                print(f"Actual: {result['actual_output']}")
                if result["error"]:
                    print(f"Error: {result['error']}")
                # Java errors include stack traces
except Exception as e:
    print(f"Error during testing: {e}")
    # Common errors:
    # - Compilation errors (syntax, type mismatches)
    # - Runtime exceptions (NullPointerException, etc.)
```

## Best Practices

1. **Always Specify Types**: Java is statically typed
   ```python
   # Always include both parameter_types and expected_type
   test_case = {
       "parameters": {"x": 5, "y": 3},
       "parameter_types": {"x": "int", "y": "int"},
       "expected": 8,
       "expected_type": "int"
   }
   ```

2. **Use Correct Java Types**: Remember Java-specific naming
   ```python
   # Correct Java types:
   "String"    # not "string"
   "boolean"   # not "bool"
   "int[]"     # not "List[int]"
   ```

3. **Handle Null Cases**: Java requires explicit null handling
   ```python
   # Consider null inputs in your test cases
   test_cases = [
       {
           "parameters": {"str": None},
           "parameter_types": {"str": "String"},
           "expected": "",
           "expected_type": "String"
       }
   ]
   ```

4. **Array vs ArrayList**: Generated code typically uses arrays
   ```python
   # Arrays are preferred over ArrayList for simplicity
   "parameter_types": {"items": "int[]"}  # not "ArrayList<Integer>"
   ```

## Common Gotchas

1. **Primitive vs Wrapper Types**: Be consistent
   ```python
   # Use primitive types when possible
   "int" not "Integer"
   "double" not "Double"
   "boolean" not "Boolean"
   ```

2. **String Comparison**: Generated code should use `.equals()`
   ```java
   // Generated code should do this:
   str1.equals(str2)  // not str1 == str2
   ```

3. **Array Initialization**: Test data is automatically converted
   ```python
   # Python list becomes Java array
   "parameters": {"arr": [1, 2, 3]}  # Becomes int[] {1, 2, 3}
   ```

## Next Steps

- Explore [C/C++ Quickstart](c-cpp.md) for another compiled language
- Learn about [Test Case Format](../guide/test-cases.md) for complex types
- See [Language Support](../guide/languages.md) for Java-specific details
- Try [Go Quickstart](go.md) for a modern compiled language comparison