---
layout: default
title: Type Mappings Reference
nav_order: 4
---

# Type Mappings Reference

This document provides a comprehensive reference for type specifications across all supported languages in the EiplGrader.

## Standard Type Strings

The following standard type strings are supported across all language executors:

### Basic Types
- `"int"` - Integer type
- `"double"` - Double-precision floating point
- `"float"` - Single-precision floating point  
- `"string"` - String/text type
- `"bool"` - Boolean type

### Array Types
- `"int[]"` - Array of integers
- `"double[]"` - Array of doubles
- `"float[]"` - Array of floats
- `"string[]"` - Array of strings
- `"bool[]"` - Array of booleans

## Language-Specific Type Mappings

### Python
Python uses dynamic typing, but for consistency with other languages:
- `"int"` → `int`
- `"double"` → `float`
- `"float"` → `float`
- `"string"` → `str`
- `"bool"` → `bool`
- `"int[]"` → `List[int]`
- `"double[]"` → `List[float]`
- `"string[]"` → `List[str]`

### JavaScript
JavaScript is dynamically typed, types are used for validation:
- `"int"` → `number`
- `"double"` → `number`
- `"float"` → `number`
- `"string"` → `string`
- `"bool"` → `boolean`
- `"int[]"` → `Array<number>`
- `"double[]"` → `Array<number>`
- `"string[]"` → `Array<string>`

### Java
- `"int"` → `int`
- `"double"` → `double`
- `"float"` → `float`
- `"string"` → `String`
- `"bool"` → `boolean`
- `"int[]"` → `int[]`
- `"double[]"` → `double[]`
- `"float[]"` → `float[]`
- `"string[]"` → `String[]`

### C
- `"int"` → `int`
- `"double"` → `double`
- `"float"` → `float`
- `"string"` → `char*`
- `"bool"` → `int` (0 or 1)
- `"int[]"` → `int*`
- `"double[]"` → `double*`
- `"float[]"` → `float*`
- `"string[]"` → `char**`

### C++
- `"int"` → `int`
- `"double"` → `double`
- `"float"` → `float`
- `"string"` → `std::string`
- `"bool"` → `bool`
- `"int[]"` → `std::vector<int>`
- `"double[]"` → `std::vector<double>`
- `"float[]"` → `std::vector<float>`
- `"string[]"` → `std::vector<std::string>`

### Go
- `"int"` → `int`
- `"double"` → `float64`
- `"float"` → `float32`
- `"string"` → `string`
- `"bool"` → `bool`
- `"int[]"` → `[]int`
- `"double[]"` → `[]float64`
- `"float[]"` → `[]float32`
- `"string[]"` → `[]string`

### Haskell
- `"int"` → `Int`
- `"double"` → `Double`
- `"float"` → `Float`
- `"string"` → `String`
- `"bool"` → `Bool`
- `"int[]"` → `[Int]`
- `"double[]"` → `[Double]`
- `"float[]"` → `[Float]`
- `"string[]"` → `[String]`

## Type Specification in Test Cases

### Recommended Format (Explicit Types)

```python
test_case = {
    "parameters": {
        "nums": [1, 2, 3],
        "target": 5
    },
    "parameter_types": {
        "nums": "int[]",
        "target": "int"
    },
    "expected": 4,
    "expected_type": "int"
}
```

### Legacy Format (Type Inference)

For backward compatibility, types can be inferred from values:

```python
test_case = {
    "parameters": {
        "nums": [1, 2, 3],  # Inferred as int[]
        "target": 5         # Inferred as int
    },
    "expected": 4          # Inferred as int
}
```

**Note**: Type inference is less reliable and slower. Explicit types are strongly recommended for compiled languages (C, C++, Java, Haskell).

## Special Considerations

### Empty Arrays
When dealing with empty arrays, explicit type specification is crucial:

```python
# Ambiguous - type cannot be inferred
{"parameters": {"arr": []}, "expected": 0}

# Clear - type is explicit
{
    "parameters": {"arr": []},
    "parameter_types": {"arr": "int[]"},
    "expected": 0,
    "expected_type": "int"
}
```

### Mixed Types in Arrays
Arrays must contain homogeneous types. Mixed-type arrays are not supported:

```python
# NOT SUPPORTED
{"parameters": {"mixed": [1, "two", 3.0]}}

# Use separate parameters instead
{
    "parameters": {
        "ints": [1, 3],
        "strings": ["two"],
        "floats": [3.0]
    },
    "parameter_types": {
        "ints": "int[]",
        "strings": "string[]",
        "floats": "double[]"
    }
}
```

### Platform-Specific Types
Some languages have platform-specific considerations:

- **C/C++**: Size of `int` may vary (use `int32_t` in generated code for consistency)
- **Go**: No implicit numeric conversions (be explicit about `int` vs `int64`)
- **Java**: Primitive arrays (`int[]`) vs object arrays (`Integer[]`)

## Error Messages

When type information is missing, executors will raise a `ValueError` with a standardized message:

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

This ensures consistent error reporting across all language executors.