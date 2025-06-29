---
layout: default
title: JavaScript Quickstart
parent: Quickstart Guides
nav_order: 2
---

# JavaScript Quickstart

Get up and running with EiplGrader for JavaScript/Node.js in minutes.

## Prerequisites

- Node.js 14+
- API key for your chosen provider (OpenAI, Meta/Llama, or Ollama local models)
- Python 3.7+ (for running EiplGrader)

## Installation

```bash
pip install eiplgrader
```

## Basic Example

### 1. Generate JavaScript Code

```python
import os
from eiplgrader.codegen import CodeGenerator

# Initialize the code generator for JavaScript
# Choose your provider: "openai", "meta", "ollama"
client_type = "openai"  # or "meta" for Llama
api_key = os.getenv("OPENAI_API_KEY")  # or META_API_KEY for Meta

generator = CodeGenerator(api_key, client_type=client_type, language="javascript")

# Generate code from a student's explanation
result = generator.generate_code(
    student_response="that takes an array of strings and returns them sorted by length",
    model="gpt-4o",  # or "Llama-4-Maverick-17B-128E-Instruct-FP8" for Meta
    function_name="sortByLength",
    gen_type="cgbg"
)

print("Generated code:")
print(result["code"][0])
```

Output:
```javascript
function sortByLength(strings) {
    return strings.sort((a, b) => a.length - b.length);
}
```

### 2. Test the Generated Code

```python
from eiplgrader.tester import CodeTester

# Define test cases - JavaScript supports automatic type inference
test_cases = [
    {
        "parameters": {"strings": ["apple", "pie", "banana", "a"]},
        "expected": ["a", "pie", "apple", "banana"]
    },
    {
        "parameters": {"strings": ["hello", "hi", "hey"]},
        "expected": ["hi", "hey", "hello"]
    },
    {
        "parameters": {"strings": []},
        "expected": []
    }
]

# Create and run the tester
tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name="sortByLength",
    language="javascript"
)

results = tester.run_tests()
print(f"Tests passed: {results.successes}/{results.testsRun}")
```

## JavaScript-Specific Features

### Async/Promise Support

JavaScript functions are automatically wrapped to handle async/promises:

```python
# Generate an async function
result = generator.generate_code(
    student_response="that fetches data and processes it asynchronously",
    model="gpt-4o",  # or your chosen model
    function_name="processData"
)

# Test cases work the same way - the test harness handles promises
test_cases = [
    {
        "parameters": {"url": "https://api.example.com/data"},
        "expected": {"processed": True}
    }
]
```

### Type Inference

JavaScript automatically infers types from test case values:

```python
test_case = {
    "parameters": {
        "count": 42,                    # number
        "price": 19.99,                 # number
        "name": "Product",              # string
        "items": [1, 2, 3],            # array of numbers
        "active": true,                 # boolean
        "data": {"key": "value"}       # object
    },
    "expected": "result"
}
```

### In-Place Modifications

JavaScript supports all three in-place modes:

```python
# Mode 0: Normal return value (default)
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "0"
}

# Mode 1: Modifies first argument in-place
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "1"  # Tests that arr is modified
}

# Mode 2: Both modifies and returns
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "2"  # Tests both modification and return value
}
```


## Advanced Example: Multiple Implementations

```python
# Generate multiple implementations
result = generator.generate_code(
    student_response="that debounces a function call",
    model="gpt-4o",  # or your chosen model
    function_name="debounce",
    num_to_gen=3  # Different debounce implementations
)

# Note: Testing debounce functions requires special handling
# due to their time-based nature
```

## Error Handling

JavaScript provides detailed error information including async errors:

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
                # JavaScript errors include stack traces
except Exception as e:
    print(f"Error during testing: {e}")
```

## Best Practices

1. **Use Modern JavaScript**: The generator typically produces ES6+ code
   ```javascript
   // Generated code uses modern features:
   // - Arrow functions
   // - Destructuring
   // - Spread operator
   // - Template literals
   ```

2. **Handle Edge Cases**: JavaScript has unique edge cases
   ```python
   test_cases = [
       {"parameters": {"arr": []}, "expected": []},           # Empty array
       {"parameters": {"arr": [undefined]}, "expected": []},  # Undefined
       {"parameters": {"arr": [null]}, "expected": []},       # Null
       {"parameters": {"arr": [NaN]}, "expected": []},        # NaN
   ]
   ```

3. **Type Coercion Awareness**: Be explicit about expected types
   ```python
   # Be careful with JavaScript's type coercion
   test_cases = [
       {"parameters": {"x": "5", "y": "3"}, "expected": 8},  # May need parseInt
       {"parameters": {"x": 5, "y": 3}, "expected": 8},      # Clear numeric
   ]
   ```

4. **Async Considerations**: The test harness handles promises automatically
   ```python
   # Both sync and async functions work the same way in tests
   test_cases = [
       {"parameters": {"data": [1, 2, 3]}, "expected": 6}
   ]
   # Works for both:
   # function sum(data) { return data.reduce((a, b) => a + b, 0); }
   # async function sum(data) { return data.reduce((a, b) => a + b, 0); }
   ```

## Common Gotchas

1. **Array/Object Equality**: Deep equality is used for testing
   ```python
   # This works correctly:
   test_case = {
       "parameters": {"obj": {"a": 1}},
       "expected": {"a": 1}  # Deep equality check
   }
   ```

2. **Floating Point Precision**: Be careful with decimal comparisons
   ```python
   # May need to round or use tolerance
   test_case = {
       "parameters": {"prices": [19.99, 29.99]},
       "expected": 49.98  # Floating point precision issues
   }
   ```

## Next Steps

- Explore [Advanced Features](../guide/advanced-features.md) like code segmentation
- Learn about [Test Case Format](../guide/test-cases.md) in detail
- Compare with [Python Quickstart](python.md) for dynamic language similarities
- Try [Java Quickstart](java.md) for static typing contrast