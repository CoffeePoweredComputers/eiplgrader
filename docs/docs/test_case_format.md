---
layout: default
title: Test Case Format Guide
nav_order: 7
---

# Test Case Format Guide

This comprehensive guide explains the test case format used in EiplGrader, including the new simplified format for dynamic languages and the explicit format for static languages.

## Quick Reference

### Simplified Format (Python, JavaScript, Go) - NEW!

```python
test_case = {
    "parameters": {
        "param1": value1,
        "param2": value2
    },
    "expected": expected_value,
    "inplace": "0",  # Optional: "0", "1", or "2"
    "function_name": "foo",  # Optional: defaults to "foo"
    "timeout": 30  # Optional: timeout in seconds
}
```

### Explicit Format (C, C++, Java, Haskell) - Required

```python
test_case = {
    "parameters": {
        "param1": value1,
        "param2": value2
    },
    "parameter_types": {
        "param1": "type1",
        "param2": "type2"
    },
    "expected": expected_value,
    "expected_type": "expected_type",
    "inplace": "0",  # Optional: "0", "1", or "2"
    "function_name": "foo",  # Optional: defaults to "foo"
    "timeout": 30  # Optional: timeout in seconds
}
```

## Language Requirements

| Language | Type Annotations Required | Type Inference |
|----------|--------------------------|----------------|
| Python   | ❌ No                    | ✅ Automatic   |
| JavaScript | ❌ No                  | ✅ Automatic   |
| Go       | ❌ No                    | ✅ Automatic   |
| C        | ✅ Yes                   | ❌ Not supported |
| C++      | ✅ Yes                   | ❌ Not supported |
| Java     | ✅ Yes                   | ❌ Not supported |
| Haskell  | ✅ Yes                   | ❌ Not supported |

## Field Descriptions

### Always Required Fields

#### `parameters`
- **Type**: Dictionary
- **Description**: Maps parameter names to their values
- **Example**: `{"x": 5, "y": [1, 2, 3]}`

#### `expected`
- **Type**: Any
- **Description**: The expected result of the function call
- **Example**: `42` or `[1, 2, 3]` or `"hello"`

### Conditionally Required Fields (Static Languages Only)

#### `parameter_types`
- **Type**: Dictionary
- **Required for**: C, C++, Java, Haskell
- **Optional for**: Python, JavaScript, Go (types are inferred)
- **Description**: Maps parameter names to their type strings
- **Example**: `{"x": "int", "y": "int[]"}`
- **Note**: See [Type Mappings Reference](type_mappings.md) for supported types

#### `expected_type`
- **Type**: String
- **Required for**: C, C++, Java, Haskell
- **Optional for**: Python, JavaScript, Go (type is inferred)
- **Description**: The type of the expected result
- **Example**: `"int"` or `"int[]"` or `"string"`

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

## Type Inference Rules

For Python, JavaScript, and Go, types are automatically inferred from values:

| Value Example | Inferred Type |
|--------------|---------------|
| `5`, `-10` | `"int"` |
| `3.14`, `-0.5` | `"double"` |
| `"hello"` | `"string"` |
| `true`, `false` | `"bool"` |
| `[1, 2, 3]` | `"List[int]"` (Python/JS) or `"[]int"` (Go) |
| `[1.0, 2.5]` | `"List[double]"` (Python/JS) or `"[]float64"` (Go) |
| `["a", "b"]` | `"List[string]"` (Python/JS) or `"[]string"` (Go) |

## Complete Examples

### Example 1: Simple Math Function

#### Simplified Format (Python/JS/Go)
```python
test_cases = [
    {
        "parameters": {"a": 5, "b": 3},
        "expected": 8
    }
]
```

#### Explicit Format (C/C++/Java/Haskell)
```python
test_cases = [
    {
        "parameters": {"a": 5, "b": 3},
        "parameter_types": {"a": "int", "b": "int"},
        "expected": 8,
        "expected_type": "int"
    }
]
```

### Example 2: Array Processing

#### Simplified Format (Python/JS/Go)
```python
test_cases = [
    {
        "parameters": {"numbers": [1, 2, 3, 4, 5]},
        "expected": 15
    }
]
```

#### Explicit Format (C/C++/Java/Haskell)
```python
test_cases = [
    {
        "parameters": {"numbers": [1, 2, 3, 4, 5]},
        "parameter_types": {"numbers": "int[]"},
        "expected": 15,
        "expected_type": "int"
    }
]
```

### Example 3: String Manipulation

```python
test_cases = [
    {
        "parameters": {
            "text": "hello world",
            "old": "world",
            "new": "universe"
        },
        "parameter_types": {
            "text": "string",
            "old": "string",
            "new": "string"
        },
        "expected": "hello universe",
        "expected_type": "string"
    }
]
```

### Example 4: In-place Modification

```python
test_cases = [
    {
        "parameters": {
            "arr": [3, 1, 4, 1, 5],
            "reverse": True
        },
        "parameter_types": {
            "arr": "int[]",
            "reverse": "bool"
        },
        "expected": [5, 1, 4, 1, 3],
        "expected_type": "int[]",
        "inplace": "1"  # Modifies arr in-place
    }
]
```

### Example 5: Mixed Types

```python
test_cases = [
    {
        "parameters": {
            "scores": [85.5, 90.0, 78.5],
            "passing_grade": 80.0,
            "include_average": True
        },
        "parameter_types": {
            "scores": "double[]",
            "passing_grade": "double",
            "include_average": "bool"
        },
        "expected": {
            "passed": 2,
            "failed": 1,
            "average": 84.67
        },
        "expected_type": "object"  # For complex return types
    }
]
```

## Language-Specific Considerations

### Dynamic Languages (Type Inference Supported)

#### Python
- **Type inference**: ✅ Automatic from values
- **Type annotations**: Optional, can override inference
- Lists are used for arrays: `[1, 2, 3]` → `List[int]`

#### JavaScript
- **Type inference**: ✅ Automatic from values
- **Type annotations**: Optional, can override inference
- All numeric types handled automatically

#### Go
- **Type inference**: ✅ Automatic from values
- **Type annotations**: Optional, can override inference
- Double maps to float64: `3.14` → `float64`
- Slices used for arrays: `[1, 2, 3]` → `[]int`

### Static Languages (Type Annotations Required)

#### Java
- **Type annotations**: Required for all parameters
- String type must be capitalized: `"string"` → `String`
- Primitive arrays preferred: `"int[]"` → `int[]`

#### C/C++
- **Type annotations**: Required for all parameters
- Strings map to different types:
  - C: `"string"` → `char*`
  - C++: `"string"` → `std::string`
- Arrays become pointers in C: `"int[]"` → `int*`

#### Haskell
- **Type annotations**: Required for all parameters
- Lists use bracket notation: `"int[]"` → `[Int]`

## Error Handling

### For Static Languages (C, C++, Java, Haskell)

If required type information is missing, you'll receive:

```
ValueError: Missing required type information:
- parameter_types not provided
- expected_type not provided

Test case must include:
{
    "parameters": {...},
    "parameter_types": {"param1": "type1", ...},
    "expected": ...,
    "expected_type": "type"
}
```

### For Dynamic Languages (Python, JavaScript, Go)

Type information is automatically inferred, so you'll only see errors for:
- Missing `parameters` field
- Missing `expected` field
- Invalid parameter values that can't be inferred

## Best Practices

1. **Use simplified format when possible** - For Python, JavaScript, and Go
2. **Specify types only when required** - For C, C++, Java, and Haskell
3. **Use standard type strings** - See [Type Mappings Reference](type_mappings.md)
4. **Include edge cases** - Empty arrays, zero values, negative numbers
5. **Document complex expected values** - Add comments explaining non-obvious results
6. **Group related test cases** - Organize by functionality being tested
7. **Use descriptive parameter names** - `numbers` instead of `arr`
8. **Be consistent** - If mixing languages, consider using explicit types for all

## Migration from Legacy Format

If you have test cases in the old format without explicit types, see the [Migration Guide](migration_guide.md) for step-by-step instructions on updating them.

## Related Documentation

- [Type Mappings Reference](type_mappings.md) - Complete type string reference
- [Migration Guide](migration_guide.md) - Updating legacy test cases
- [Error Messages](error_messages.md) - Understanding error outputs
- [Tester Module](tester.md) - CodeTester API documentation