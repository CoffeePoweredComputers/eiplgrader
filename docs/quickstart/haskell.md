---
layout: default
title: Haskell Quickstart
parent: Quickstart Guides
nav_order: 6
---

# Haskell Quickstart

Get up and running with EiplGrader for Haskell in minutes.

## Prerequisites

- GHC (Glasgow Haskell Compiler) 8.0+
- Python 3.7+ (for running EiplGrader)
- OpenAI API key (or compatible LLM API)

## Installation

```bash
pip install eiplgrader
```

## Basic Example

### 1. Generate Haskell Code

```python
from eiplgrader.codegen import CodeGenerator

# Initialize the code generator for Haskell
api_key = "your-openai-api-key"
generator = CodeGenerator(api_key, client_type="openai", language="haskell")

# Generate code from a student's explanation
result = generator.generate_code(
    student_response="that doubles each element in a list",
    function_name="doubleList",
    gen_type="cgbg"
)

print("Generated code:")
print(result["code"][0])
```

Output:
```haskell
doubleList :: [Int] -> [Int]
doubleList xs = map (*2) xs
```

### 2. Test the Generated Code

```python
from eiplgrader.tester import CodeTester

# Define test cases - Haskell REQUIRES explicit type annotations
test_cases = [
    {
        "parameters": {"xs": [1, 2, 3, 4, 5]},
        "parameter_types": {"xs": "[Int]"},  # Haskell list type
        "expected": [2, 4, 6, 8, 10],
        "expected_type": "[Int]"
    },
    {
        "parameters": {"xs": []},
        "parameter_types": {"xs": "[Int]"},
        "expected": [],
        "expected_type": "[Int]"
    },
    {
        "parameters": {"xs": [-1, 0, 1]},
        "parameter_types": {"xs": "[Int]"},
        "expected": [-2, 0, 2],
        "expected_type": "[Int]"
    }
]

# Create and run the tester
tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name="doubleList",
    language="haskell"
)

results = tester.run_tests()
print(f"Tests passed: {results.successes}/{results.testsRun}")
```

## Haskell-Specific Features

### Pure Functional Programming

Haskell is purely functional, so all functions are pure by default:

```python
# Generate pure functions
result = generator.generate_code(
    student_response="that calculates factorial using recursion",
    function_name="factorial"
)

# Output includes type signature
# factorial :: Int -> Int
# factorial 0 = 1
# factorial n = n * factorial (n - 1)
```

### Type Annotations are REQUIRED

Haskell requires explicit type information for test cases:

```python
# CORRECT - with type annotations
test_case = {
    "parameters": {"x": 5, "y": 3},
    "parameter_types": {"x": "Int", "y": "Int"},  # Required!
    "expected": 8,
    "expected_type": "Int"  # Required!
}

# WRONG - missing type annotations
test_case = {
    "parameters": {"x": 5, "y": 3},
    "expected": 8
    # This will fail with: "Missing required type information"
}
```

### Haskell Type Mappings

| Generic Type | Haskell Type | Example |
|-------------|--------------|---------|
| `int` | `Int` | `42` |
| `double` | `Double` | `3.14` |
| `string` | `String` | `"hello"` |
| `bool` | `Bool` | `True` |
| `List[int]` | `[Int]` | `[1, 2, 3]` |
| `List[string]` | `[String]` | `["a", "b"]` |

### Type Signatures

Generated Haskell code includes type signatures:

```haskell
-- Function with multiple parameters
add :: Int -> Int -> Int
add x y = x + y

-- Function with lists
sumList :: [Int] -> Int
sumList xs = sum xs

-- Function with strings
greet :: String -> String
greet name = "Hello, " ++ name
```

## Common Haskell Patterns

### List Comprehensions
```python
result = generator.generate_code(
    student_response="that generates all even squares up to n",
    function_name="evenSquares"
)

test_cases = [
    {
        "parameters": {"n": 20},
        "parameter_types": {"n": "Int"},
        "expected": [4, 16],
        "expected_type": "[Int]"
    }
]
```

### Higher-Order Functions
```python
result = generator.generate_code(
    student_response="that filters a list keeping only positive numbers",
    function_name="keepPositive"
)

test_cases = [
    {
        "parameters": {"nums": [-2, -1, 0, 1, 2, 3]},
        "parameter_types": {"nums": "[Int]"},
        "expected": [1, 2, 3],
        "expected_type": "[Int]"
    }
]
```

### Pattern Matching
```python
result = generator.generate_code(
    student_response="that returns the head of a list or a default value if empty",
    function_name="safeHead"
)

test_cases = [
    {
        "parameters": {"xs": [1, 2, 3], "default": 0},
        "parameter_types": {"xs": "[Int]", "default": "Int"},
        "expected": 1,
        "expected_type": "Int"
    },
    {
        "parameters": {"xs": [], "default": 0},
        "parameter_types": {"xs": "[Int]", "default": "Int"},
        "expected": 0,
        "expected_type": "Int"
    }
]
```

### Recursion
```python
result = generator.generate_code(
    student_response="that calculates the nth Fibonacci number",
    function_name="fibonacci"
)

test_cases = [
    {
        "parameters": {"n": 0},
        "parameter_types": {"n": "Int"},
        "expected": 0,
        "expected_type": "Int"
    },
    {
        "parameters": {"n": 10},
        "parameter_types": {"n": "Int"},
        "expected": 55,
        "expected_type": "Int"
    }
]
```

## Working with Different Types

### Strings
```python
result = generator.generate_code(
    student_response="that reverses each word in a sentence",
    function_name="reverseWords"
)

test_cases = [
    {
        "parameters": {"sentence": "Hello World"},
        "parameter_types": {"sentence": "String"},
        "expected": "olleH dlroW",
        "expected_type": "String"
    }
]
```

### Tuples
```python
# Note: Tuple handling is limited in test cases
result = generator.generate_code(
    student_response="that returns the minimum and maximum of a list",
    function_name="minMax"
)

# Test the first element of the tuple
test_cases = [
    {
        "parameters": {"xs": [3, 1, 4, 1, 5]},
        "parameter_types": {"xs": "[Int]"},
        "expected": 1,  # Testing minimum only
        "expected_type": "Int"
    }
]
```

### Custom Types
```python
# Basic types work best with the test harness
result = generator.generate_code(
    student_response="that counts occurrences of each element",
    function_name="countElements"
)

# Return as a list of pairs rather than custom types
test_cases = [
    {
        "parameters": {"xs": [1, 2, 2, 3, 3, 3]},
        "parameter_types": {"xs": "[Int]"},
        "expected": [[1, 1], [2, 2], [3, 3]],
        "expected_type": "[[Int]]"
    }
]
```

## Lazy Evaluation

Haskell's lazy evaluation allows working with infinite lists:

```python
result = generator.generate_code(
    student_response="that takes the first n prime numbers",
    function_name="firstPrimes"
)

test_cases = [
    {
        "parameters": {"n": 5},
        "parameter_types": {"n": "Int"},
        "expected": [2, 3, 5, 7, 11],
        "expected_type": "[Int]"
    }
]
```

## Module Structure

The test harness creates a complete Haskell module:

```haskell
module Main where

-- Your generated function
doubleList :: [Int] -> [Int]
doubleList xs = map (*2) xs

-- Test harness main function
main :: IO ()
main = do
    -- Test execution code
    print (doubleList [1, 2, 3])
```

## Best Practices

1. **Use explicit type signatures**:
   ```python
   # Always include Haskell types
   "parameter_types": {
       "xs": "[Int]",     # List of Int
       "x": "Int",        # Single Int
       "str": "String",   # String type
       "flag": "Bool"     # Boolean
   }
   ```

2. **Prefer simple types**:
   ```python
   # Stick to basic types for test cases
   # Good: Int, Double, String, Bool, [Int], [String]
   # Avoid: Complex ADTs, IO types, Monads
   ```

3. **Functional style**:
   ```haskell
   -- Generated code should use:
   -- map, filter, fold
   -- Pattern matching
   -- Guards
   -- List comprehensions
   ```

4. **Handle edge cases functionally**:
   ```python
   # Empty lists, zero values
   test_cases = [
       {"parameters": {"xs": []}, "expected": 0},
       {"parameters": {"n": 0}, "expected": 1}
   ]
   ```

## Common Gotchas

1. **No side effects**: Haskell functions must be pure
   ```python
   # Cannot test functions with IO or side effects
   # Stick to pure transformations
   ```

2. **Type inference**: Test harness requires explicit types
   ```python
   # Even though Haskell can infer types,
   # test cases must specify them explicitly
   ```

3. **Infinite lists**: Be careful with lazy evaluation
   ```python
   # Always use 'take' or similar to limit infinite lists
   ```

4. **String as [Char]**: Remember String is a list
   ```haskell
   -- String is [Char] in Haskell
   -- This affects some operations
   ```

## Function Name Detection

The executor can automatically detect function names from type signatures:

```python
# If function_name is not specified, it's extracted from the code
result = generator.generate_code(
    student_response="that zips two lists together",
    gen_type="cgbg"
    # function_name omitted - will be detected
)
```

## Compilation and Execution

- **Compilation**: `ghc -o output input.hs`
- **Module**: Always creates `module Main`
- **Entry point**: `main :: IO ()` added automatically

## Error Handling

Haskell provides detailed type errors:

```python
try:
    results = tester.run_tests()
    if not results.was_successful():
        for result in results.test_results:
            if not result["pass"]:
                print(f"Test failed: {result['function_call']}")
            # Common Haskell errors:
            # - Type mismatches
            # - Pattern match failures
            # - Undefined functions
except Exception as e:
    print(f"Error: {e}")
```

## Advanced Example

```python
# Generate a more complex function
result = generator.generate_code(
    student_response="""that implements quicksort algorithm 
                         for a list of integers""",
    function_name="quicksort"
)

test_cases = [
    {
        "parameters": {"xs": [3, 1, 4, 1, 5, 9, 2, 6]},
        "parameter_types": {"xs": "[Int]"},
        "expected": [1, 1, 2, 3, 4, 5, 6, 9],
        "expected_type": "[Int]"
    },
    {
        "parameters": {"xs": []},
        "parameter_types": {"xs": "[Int]"},
        "expected": [],
        "expected_type": "[Int]"
    },
    {
        "parameters": {"xs": [1]},
        "parameter_types": {"xs": "[Int]"},
        "expected": [1],
        "expected_type": "[Int]"
    }
]
```

## Next Steps

- Compare with [Go Quickstart](go.md) for another functional-friendly language
- Learn about [Test Case Format](../guide/test-cases.md) for complex types
- See [Language Support](../guide/languages.md) for Haskell-specific details
- Explore functional patterns in [Advanced Features](../guide/advanced-features.md)