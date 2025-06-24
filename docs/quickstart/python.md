---
layout: default
title: Python Quickstart
parent: Quickstart Guides
nav_order: 1
---

# Python Quickstart

Get up and running with EiplGrader for Python in minutes.

## Prerequisites

- Python 3.7+
- OpenAI API key (or compatible LLM API)

## Installation

```bash
pip install eiplgrader
```

## Basic Example

### 1. Generate Code from Natural Language

```python
from eiplgrader.codegen import CodeGenerator

# Initialize the code generator
api_key = "your-openai-api-key"
generator = CodeGenerator(api_key, client_type="openai", language="python")

# Generate code from a student's explanation
result = generator.generate_code(
    student_response="that takes a list of numbers and returns only the even ones",
    function_name="filter_even",
    gen_type="cgbg"
)

print("Generated code:")
print(result["code"][0])
```

Output:
```python
def filter_even(numbers):
    return [num for num in numbers if num % 2 == 0]
```

### 2. Test the Generated Code

```python
from eiplgrader.tester import CodeTester

# Define test cases - Python supports automatic type inference
test_cases = [
    {
        "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
        "expected": [2, 4, 6]
    },
    {
        "parameters": {"numbers": [1, 3, 5, 7]},
        "expected": []
    },
    {
        "parameters": {"numbers": []},
        "expected": []
    }
]

# Create and run the tester
tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name="filter_even",
    language="python"
)

results = tester.run_tests()
print(f"Tests passed: {results.successes}/{results.testsRun}")
```

## Python-Specific Features

### Type Inference

Python automatically infers types from test case values:

```python
# These types are automatically inferred:
test_case = {
    "parameters": {
        "x": 42,              # Inferred as int
        "y": 3.14,            # Inferred as float
        "name": "Alice",      # Inferred as str
        "items": [1, 2, 3],   # Inferred as List[int]
        "flag": True          # Inferred as bool
    },
    "expected": "result"      # Inferred as str
}
```

### In-Place Modifications

Python supports all three in-place modes:

```python
# Mode 0: Normal return value (default)
test_case = {
    "parameters": {"numbers": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "0"
}

# Mode 1: Modifies first argument in-place
test_case = {
    "parameters": {"numbers": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "1"  # Tests that numbers list is modified
}

# Mode 2: Both modifies and returns
test_case = {
    "parameters": {"numbers": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "2"  # Tests both modification and return value
}
```

### Advanced Example: Multiple Functions

```python
# Generate multiple implementations
result = generator.generate_code(
    student_response="that calculates the factorial of a number",
    function_name="factorial",
    num_to_gen=3  # Generate 3 different implementations
)

# Test all implementations
for i, code in enumerate(result["code"]):
    print(f"\nImplementation {i + 1}:")
    print(code)
    
    tester = CodeTester(
        code=code,
        test_cases=[
            {"parameters": {"n": 5}, "expected": 120},
            {"parameters": {"n": 0}, "expected": 1},
            {"parameters": {"n": 1}, "expected": 1}
        ],
        function_name="factorial",
        language="python"
    )
    
    results = tester.run_tests()
    print(f"Tests passed: {results.successes}/{results.testsRun}")
```

## Common Patterns

### String Processing
```python
result = generator.generate_code(
    student_response="that reverses each word in a sentence but keeps word order",
    function_name="reverse_words"
)

test_cases = [
    {
        "parameters": {"sentence": "Hello World"},
        "expected": "olleH dlroW"
    }
]
```

### List Manipulation
```python
result = generator.generate_code(
    student_response="that finds the second largest number in a list",
    function_name="second_largest"
)

test_cases = [
    {
        "parameters": {"numbers": [10, 20, 4, 45, 99]},
        "expected": 45
    }
]
```

### Dictionary Operations
```python
result = generator.generate_code(
    student_response="that counts the frequency of each character in a string",
    function_name="char_frequency"
)

test_cases = [
    {
        "parameters": {"text": "hello"},
        "expected": {"h": 1, "e": 1, "l": 2, "o": 1}
    }
]
```

## Error Handling

Python provides detailed error information:

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
except Exception as e:
    print(f"Error during testing: {e}")
```

## Best Practices

1. **Clear Descriptions**: Provide specific, unambiguous descriptions
   ```python
   # Good
   "that returns the sum of all even numbers in a list"
   
   # Too vague
   "that processes numbers"
   ```

2. **Edge Cases**: Always include edge case tests
   ```python
   test_cases = [
       {"parameters": {"lst": []}, "expected": 0},  # Empty list
       {"parameters": {"lst": [1]}, "expected": 1},  # Single element
       {"parameters": {"lst": None}, "expected": 0}  # None handling
   ]
   ```

3. **Type Consistency**: While Python infers types, maintain consistency
   ```python
   # Consistent types across test cases
   test_cases = [
       {"parameters": {"x": 1}, "expected": 2},
       {"parameters": {"x": 5}, "expected": 10},
       # Not: {"parameters": {"x": "1"}, "expected": "2"}
   ]
   ```

## Next Steps

- Explore [Advanced Features](../guide/advanced-features.md) like code segmentation
- Learn about [Test Case Format](../guide/test-cases.md) in detail
- See examples for [other languages](../quickstart/)