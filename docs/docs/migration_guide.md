---
layout: default
title: Migration Guide
nav_order: 5
---

# Migration Guide: Test Case Format

This guide helps you migrate from the legacy test case format (type inference) to the new explicit type format.

## Why Migrate?

The new format with explicit types provides:
- **Better Performance**: No runtime type inference needed
- **Improved Reliability**: Explicit types prevent ambiguity
- **Cross-Language Support**: Essential for compiled languages
- **Clear Documentation**: Test cases are self-documenting

## Format Comparison

### Legacy Format (Type Inference)
```python
test_cases = [
    {
        "parameters": {"a": 5, "b": 3},
        "expected": 8
    },
    {
        "parameters": {"nums": [1, 2, 3]},
        "expected": 6
    }
]
```

### New Format (Explicit Types)
```python
test_cases = [
    {
        "parameters": {"a": 5, "b": 3},
        "parameter_types": {"a": "int", "b": "int"},
        "expected": 8,
        "expected_type": "int"
    },
    {
        "parameters": {"nums": [1, 2, 3]},
        "parameter_types": {"nums": "int[]"},
        "expected": 6,
        "expected_type": "int"
    }
]
```

## Migration Steps

### Step 1: Add `parameter_types`

For each test case, add a `parameter_types` dictionary that maps parameter names to their types:

```python
# Before
{"parameters": {"x": 10, "y": 20}}

# After
{
    "parameters": {"x": 10, "y": 20},
    "parameter_types": {"x": "int", "y": "int"}
}
```

### Step 2: Add `expected_type`

Specify the type of the expected result:

```python
# Before
{"expected": True}

# After
{
    "expected": True,
    "expected_type": "bool"
}
```

### Step 3: Handle Special Cases

#### Empty Arrays
```python
# Before (ambiguous)
{"parameters": {"arr": []}, "expected": 0}

# After (explicit)
{
    "parameters": {"arr": []},
    "parameter_types": {"arr": "int[]"},  # Specify element type
    "expected": 0,
    "expected_type": "int"
}
```

#### String Parameters
```python
# Before
{"parameters": {"name": "Alice"}, "expected": "Hello, Alice!"}

# After
{
    "parameters": {"name": "Alice"},
    "parameter_types": {"name": "string"},
    "expected": "Hello, Alice!",
    "expected_type": "string"
}
```

## Common Migration Patterns

### Pattern 1: Simple Math Function
```python
# Legacy
test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 3},
    {"parameters": {"a": -5, "b": 5}, "expected": 0}
]

# Migrated
test_cases = [
    {
        "parameters": {"a": 1, "b": 2},
        "parameter_types": {"a": "int", "b": "int"},
        "expected": 3,
        "expected_type": "int"
    },
    {
        "parameters": {"a": -5, "b": 5},
        "parameter_types": {"a": "int", "b": "int"},
        "expected": 0,
        "expected_type": "int"
    }
]
```

### Pattern 2: Array Processing
```python
# Legacy
test_cases = [
    {"parameters": {"numbers": [1, 2, 3, 4]}, "expected": 10},
    {"parameters": {"numbers": []}, "expected": 0}
]

# Migrated
test_cases = [
    {
        "parameters": {"numbers": [1, 2, 3, 4]},
        "parameter_types": {"numbers": "int[]"},
        "expected": 10,
        "expected_type": "int"
    },
    {
        "parameters": {"numbers": []},
        "parameter_types": {"numbers": "int[]"},
        "expected": 0,
        "expected_type": "int"
    }
]
```

### Pattern 3: Mixed Types
```python
# Legacy
test_cases = [
    {
        "parameters": {
            "text": "hello",
            "count": 3,
            "separator": " "
        },
        "expected": "hello hello hello"
    }
]

# Migrated
test_cases = [
    {
        "parameters": {
            "text": "hello",
            "count": 3,
            "separator": " "
        },
        "parameter_types": {
            "text": "string",
            "count": "int",
            "separator": "string"
        },
        "expected": "hello hello hello",
        "expected_type": "string"
    }
]
```

## Automated Migration Helper

Here's a simple script to help migrate your test cases:

```python
def migrate_test_case(old_test_case):
    """Migrate a legacy test case to the new format."""
    new_test_case = old_test_case.copy()
    
    # Infer parameter types
    if "parameter_types" not in new_test_case and "parameters" in new_test_case:
        parameter_types = {}
        for name, value in new_test_case["parameters"].items():
            if isinstance(value, bool):
                parameter_types[name] = "bool"
            elif isinstance(value, int):
                parameter_types[name] = "int"
            elif isinstance(value, float):
                parameter_types[name] = "double"
            elif isinstance(value, str):
                parameter_types[name] = "string"
            elif isinstance(value, list):
                if not value:
                    # Default to int[] for empty arrays
                    parameter_types[name] = "int[]"
                elif isinstance(value[0], int):
                    parameter_types[name] = "int[]"
                elif isinstance(value[0], float):
                    parameter_types[name] = "double[]"
                elif isinstance(value[0], str):
                    parameter_types[name] = "string[]"
        
        new_test_case["parameter_types"] = parameter_types
    
    # Infer expected type
    if "expected_type" not in new_test_case and "expected" in new_test_case:
        expected = new_test_case["expected"]
        if isinstance(expected, bool):
            new_test_case["expected_type"] = "bool"
        elif isinstance(expected, int):
            new_test_case["expected_type"] = "int"
        elif isinstance(expected, float):
            new_test_case["expected_type"] = "double"
        elif isinstance(expected, str):
            new_test_case["expected_type"] = "string"
        elif isinstance(expected, list):
            if not expected:
                new_test_case["expected_type"] = "int[]"
            elif isinstance(expected[0], int):
                new_test_case["expected_type"] = "int[]"
            elif isinstance(expected[0], float):
                new_test_case["expected_type"] = "double[]"
            elif isinstance(expected[0], str):
                new_test_case["expected_type"] = "string[]"
    
    return new_test_case

# Example usage
old_test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 3},
    {"parameters": {"arr": [1, 2, 3]}, "expected": 6}
]

new_test_cases = [migrate_test_case(tc) for tc in old_test_cases]
```

## Validation

After migration, validate your test cases:

```python
def validate_test_case(test_case):
    """Validate that a test case has all required fields."""
    required_fields = ["parameters", "expected"]
    recommended_fields = ["parameter_types", "expected_type"]
    
    # Check required fields
    for field in required_fields:
        if field not in test_case:
            raise ValueError(f"Missing required field: {field}")
    
    # Check recommended fields
    missing_recommended = []
    for field in recommended_fields:
        if field not in test_case:
            missing_recommended.append(field)
    
    if missing_recommended:
        print(f"Warning: Missing recommended fields: {missing_recommended}")
        print("Test case will use type inference (slower, less reliable)")
    
    # Validate parameter_types matches parameters
    if "parameter_types" in test_case:
        for param in test_case["parameters"]:
            if param not in test_case["parameter_types"]:
                raise ValueError(f"Missing type for parameter: {param}")
    
    return True
```

## Backward Compatibility

The system maintains backward compatibility:
- Python executor will continue to work with type inference
- JavaScript executor requires explicit types but provides clear error messages
- Compiled language executors (C, C++, Java, Haskell) strongly benefit from explicit types

## Best Practices

1. **Always specify types explicitly** for new test cases
2. **Migrate existing test cases** when updating tests
3. **Use the type mapping reference** to ensure correct type strings
4. **Test your migrated cases** to ensure they work correctly
5. **Document type requirements** in your test suite documentation