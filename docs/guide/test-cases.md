---
layout: default
title: Test Case Format
parent: User Guide
nav_order: 3
---

# Test Case Format Guide

Comprehensive guide to writing test cases for all supported languages in EiplGrader.

## Quick Reference

### Dynamic Languages (Python, JavaScript)
```python
test_case = {
    "parameters": {"x": 5, "y": 3},
    "expected": 8,
    "inplace": "0",  # Optional
    "function_name": "add",  # Optional
    "timeout": 30  # Optional
}
```

### Static Languages (C, C++, Java, Go, Haskell)
```python
test_case = {
    "parameters": {"x": 5, "y": 3},
    "parameter_types": {"x": "int", "y": "int"},  # Required!
    "expected": 8,
    "expected_type": "int",  # Required!
    "inplace": "0",  # Optional
    "function_name": "add",  # Optional
    "timeout": 30  # Optional
}
```

## Field Descriptions

### Always Required Fields

#### `parameters`
- **Type**: Dictionary
- **Description**: Maps parameter names to their values
- **Example**: `{"x": 5, "y": [1, 2, 3], "name": "test"}`

#### `expected`
- **Type**: Any
- **Description**: The expected result of the function call
- **Example**: `42`, `[1, 2, 3]`, `"hello"`, `{"key": "value"}`

### Conditionally Required Fields

#### `parameter_types` (Static Languages Only)
- **Required for**: C, C++, Java, Go, Haskell
- **Type**: Dictionary
- **Description**: Maps parameter names to their type strings
- **Example**: `{"x": "int", "y": "int[]", "name": "String"}`

#### `expected_type` (Static Languages Only)
- **Required for**: C, C++, Java, Go, Haskell
- **Type**: String
- **Description**: The type of the expected result
- **Example**: `"int"`, `"String"`, `"std::vector<int>"`

### Optional Fields

#### `inplace`
- **Type**: String
- **Default**: `"0"`
- **Options**:
  - `"0"`: Function returns a value (default)
  - `"1"`: Function modifies first argument in-place
  - `"2"`: Function both modifies and returns

#### `function_name`
- **Type**: String
- **Default**: `"foo"`
- **Description**: Name of the function to test

#### `timeout`
- **Type**: Number
- **Default**: `30`
- **Description**: Maximum execution time in seconds

## Type Inference (Dynamic Languages)

Python and JavaScript automatically infer types from values:

| Value Example | Python Type | JavaScript Type |
|--------------|-------------|-----------------|
| `42` | `int` | `number` |
| `3.14` | `float` | `number` |
| `"hello"` | `str` | `string` |
| `True`/`true` | `bool` | `boolean` |
| `[1, 2, 3]` | `List[int]` | `Array` |
| `{"a": 1}` | `dict` | `Object` |

### Python Type Inference Examples

```python
# Integer
{"parameters": {"x": 42}, "expected": 84}
# Inferred: x is int, return is int

# Float
{"parameters": {"x": 3.14}, "expected": 6.28}
# Inferred: x is float, return is float

# String
{"parameters": {"text": "hello"}, "expected": "HELLO"}
# Inferred: text is str, return is str

# List
{"parameters": {"numbers": [1, 2, 3]}, "expected": 6}
# Inferred: numbers is List[int], return is int

# Mixed types
{
    "parameters": {
        "name": "Alice",
        "age": 25,
        "scores": [90, 85, 88]
    },
    "expected": "Alice (25): avg 87.67"
}
# Inferred: name is str, age is int, scores is List[int]
```

## Type Annotations (Static Languages)

### Java Type Mappings

```python
# Basic types
test_case = {
    "parameters": {
        "x": 42,
        "price": 19.99,
        "name": "Product",
        "active": True
    },
    "parameter_types": {
        "x": "int",
        "price": "double",
        "name": "String",  # Note: capital S
        "active": "boolean"
    },
    "expected": "Product: $19.99",
    "expected_type": "String"
}

# Arrays
test_case = {
    "parameters": {
        "numbers": [1, 2, 3, 4, 5],
        "names": ["Alice", "Bob"]
    },
    "parameter_types": {
        "numbers": "int[]",
        "names": "String[]"
    },
    "expected": 15,
    "expected_type": "int"
}
```

### C++ Type Mappings

```python
# STL containers
test_case = {
    "parameters": {
        "nums": [1, 2, 3],
        "text": "hello",
        "values": [1.1, 2.2, 3.3]
    },
    "parameter_types": {
        "nums": "std::vector<int>",
        "text": "std::string",
        "values": "std::vector<double>"
    },
    "expected": [3, 2, 1],
    "expected_type": "std::vector<int>"
}
```

### C Type Mappings

```python
# C requires array size
test_case = {
    "parameters": {
        "arr": [10, 20, 30],
        "n": 3,  # Array size
        "str": "hello"
    },
    "parameter_types": {
        "arr": "int*",
        "n": "int",
        "str": "char*"
    },
    "expected": 60,
    "expected_type": "int"
}
```

### Go Type Mappings

```python
# Go slices and maps
test_case = {
    "parameters": {
        "numbers": [1, 2, 3],
        "prices": [10.5, 20.0],
        "data": {"a": 1, "b": 2}
    },
    "parameter_types": {
        "numbers": "[]int",
        "prices": "[]float64",  # Note: float64, not double
        "data": "map[string]int"
    },
    "expected": 6,
    "expected_type": "int"
}
```

### Haskell Type Mappings

```python
# Haskell lists and types
test_case = {
    "parameters": {
        "xs": [1, 2, 3],
        "x": 42,
        "str": "hello"
    },
    "parameter_types": {
        "xs": "[Int]",  # List notation
        "x": "Int",     # Capital I
        "str": "String"
    },
    "expected": [2, 4, 6],
    "expected_type": "[Int]"
}
```

## Complex Test Cases

### Testing Functions with Multiple Return Values

Most languages only test the first return value:

```python
# Go function that returns (quotient, remainder)
test_case = {
    "parameters": {"a": 17, "b": 5},
    "parameter_types": {"a": "int", "b": "int"},
    "expected": 3,  # Only testing quotient
    "expected_type": "int"
}
```

### Testing Object/Dictionary Returns

```python
# Python - Dictionary return
test_case = {
    "parameters": {"data": [1, 2, 2, 3, 3, 3]},
    "expected": {"1": 1, "2": 2, "3": 3}
}

# JavaScript - Object return
test_case = {
    "parameters": {"items": ["a", "b", "a"]},
    "expected": {"a": 2, "b": 1}
}

# Java - Requires serialization approach
# Often better to test a specific aspect
test_case = {
    "parameters": {"data": [1, 2, 3]},
    "parameter_types": {"data": "int[]"},
    "expected": "min:1,max:3,avg:2.0",  # String representation
    "expected_type": "String"
}
```

### Testing In-Place Modifications

#### Mode 0: Normal Return
```python
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "0"  # Default - expects sorted array returned
}
```

#### Mode 1: In-Place Only
```python
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "1"  # Checks that arr is modified, no return value
}
```

#### Mode 2: In-Place and Return
```python
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "2"  # Checks both modification and return
}
```

## Language-Specific Examples

### Python: Rich Data Types

```python
# Lists of lists
test_case = {
    "parameters": {"matrix": [[1, 2], [3, 4]]},
    "expected": [[1, 3], [2, 4]]  # Transpose
}

# Mixed containers
test_case = {
    "parameters": {
        "data": [
            {"name": "Alice", "score": 85},
            {"name": "Bob", "score": 92}
        ]
    },
    "expected": 88.5  # Average score
}

# Nested structures
test_case = {
    "parameters": {
        "tree": {
            "value": 10,
            "left": {"value": 5},
            "right": {"value": 15}
        }
    },
    "expected": 30  # Sum of all values
}
```

### JavaScript: JSON-Compatible Types

```python
# Arrays and objects
test_case = {
    "parameters": {
        "users": [
            {"id": 1, "active": True},
            {"id": 2, "active": False}
        ]
    },
    "expected": [1]  # Active user IDs
}

# Nested objects
test_case = {
    "parameters": {
        "config": {
            "server": {
                "host": "localhost",
                "port": 8080
            }
        }
    },
    "expected": "http://localhost:8080"
}
```

### Java: Typed Collections

```python
# Integer array operations
test_case = {
    "parameters": {"arr": [5, 2, 8, 1, 9]},
    "parameter_types": {"arr": "int[]"},
    "expected": 5,  # Median
    "expected_type": "int"
}

# String array operations
test_case = {
    "parameters": {
        "words": ["hello", "world", "java"],
        "separator": ", "
    },
    "parameter_types": {
        "words": "String[]",
        "separator": "String"
    },
    "expected": "hello, world, java",
    "expected_type": "String"
}

# 2D arrays
test_case = {
    "parameters": {"matrix": [[1, 2, 3], [4, 5, 6]]},
    "parameter_types": {"matrix": "int[][]"},
    "expected": 21,
    "expected_type": "int"
}
```

### C: Pointer-Based Arrays

```python
# Array with size
test_case = {
    "parameters": {
        "arr": [10, 20, 30, 40, 50],
        "n": 5
    },
    "parameter_types": {
        "arr": "int*",
        "n": "int"
    },
    "expected": 30,  # Average
    "expected_type": "int"
}

# String operations
test_case = {
    "parameters": {
        "str1": "hello",
        "str2": "world"
    },
    "parameter_types": {
        "str1": "char*",
        "str2": "char*"
    },
    "expected": "helloworld",
    "expected_type": "char*"
}
```

### C++: STL Containers

```python
# Vector operations
test_case = {
    "parameters": {"vec": [3, 1, 4, 1, 5, 9]},
    "parameter_types": {"vec": "std::vector<int>"},
    "expected": [1, 1, 3, 4, 5, 9],
    "expected_type": "std::vector<int>"
}

# Map operations
test_case = {
    "parameters": {
        "vec": [1, 2, 2, 3, 3, 3]
    },
    "parameter_types": {
        "vec": "std::vector<int>"
    },
    "expected": {"1": 1, "2": 2, "3": 3},
    "expected_type": "std::map<std::string, int>"
}
```

### Go: Slices and Maps

```python
# Slice operations
test_case = {
    "parameters": {"nums": [1, 2, 3, 4, 5]},
    "parameter_types": {"nums": "[]int"},
    "expected": [2, 4],  # Even numbers
    "expected_type": "[]int"
}

# Map operations
test_case = {
    "parameters": {
        "words": ["go", "python", "go", "java", "go"]
    },
    "parameter_types": {"words": "[]string"},
    "expected": {"go": 3, "python": 1, "java": 1},
    "expected_type": "map[string]int"
}
```

### Haskell: Functional Types

```python
# List transformations
test_case = {
    "parameters": {"xs": [1, 2, 3, 4, 5]},
    "parameter_types": {"xs": "[Int]"},
    "expected": [1, 4, 9, 16, 25],  # Squares
    "expected_type": "[Int]"
}

# String operations
test_case = {
    "parameters": {"words": ["hello", "world"]},
    "parameter_types": {"words": "[String]"},
    "expected": ["HELLO", "WORLD"],
    "expected_type": "[String]"
}
```

## Best Practices

### 1. Include Edge Cases

```python
test_cases = [
    # Normal case
    {"parameters": {"lst": [1, 2, 3]}, "expected": 6},
    # Empty input
    {"parameters": {"lst": []}, "expected": 0},
    # Single element
    {"parameters": {"lst": [42]}, "expected": 42},
    # Negative numbers
    {"parameters": {"lst": [-1, -2, -3]}, "expected": -6},
    # Large numbers
    {"parameters": {"lst": [1000000, 2000000]}, "expected": 3000000}
]
```

### 2. Test Boundary Conditions

```python
# Integer boundaries
test_cases = [
    {"parameters": {"x": 0}, "expected": 0},
    {"parameters": {"x": -1}, "expected": 1},
    {"parameters": {"x": 2147483647}, "expected": 2147483647}  # Max int
]

# Array boundaries
test_cases = [
    {"parameters": {"arr": [], "n": 0}, "expected": -1},  # Empty
    {"parameters": {"arr": [1], "n": 1}, "expected": 0},  # Single
    {"parameters": {"arr": [1]*1000, "n": 1000}, "expected": 0}  # Large
]
```

### 3. Use Descriptive Test Data

```python
# Good - Clear what's being tested
test_cases = [
    {
        "parameters": {"prices": [10.00, 20.00, 15.00]},
        "expected": 15.00  # Average price
    }
]

# Less clear
test_cases = [
    {
        "parameters": {"x": [10, 20, 15]},
        "expected": 15
    }
]
```

### 4. Consider Type Consistency

```python
# Consistent types across test cases
test_cases = [
    {"parameters": {"x": 1.0}, "expected": 2.0},
    {"parameters": {"x": 2.5}, "expected": 5.0},
    {"parameters": {"x": 0.0}, "expected": 0.0}
]

# Avoid mixing types
# Bad: {"parameters": {"x": 1}, "expected": 2.0}  # int -> float
```

## Common Errors and Solutions

### Missing Type Information

```python
# Error: Missing required type information
# Fix for static languages:
test_case = {
    "parameters": {"x": 5},
    "parameter_types": {"x": "int"},  # Add this
    "expected": 10,
    "expected_type": "int"  # Add this
}
```

### Type Mismatches

```python
# Error: Type mismatch
# Java: expecting String not string
test_case = {
    "parameter_types": {"name": "String"},  # Not "string"
    "expected_type": "String"  # Capital S
}

# C++: proper STL types
test_case = {
    "parameter_types": {"vec": "std::vector<int>"},  # Full namespace
    "expected_type": "std::vector<int>"
}
```

### Array Size Issues (C)

```python
# Error: Segmentation fault
# Fix: Always provide array size
test_case = {
    "parameters": {"arr": [1, 2, 3], "n": 3},  # Include size
    "parameter_types": {"arr": "int*", "n": "int"}
}
```

## Next Steps

- See [Language Support](languages.md) for language-specific details
- Explore [Advanced Features](advanced-features.md) for complex testing
- Learn about [Error Handling](errors.md) for debugging test failures