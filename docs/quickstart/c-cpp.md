---
layout: default
title: C/C++ Quickstart
parent: Quickstart Guides
nav_order: 4
---

# C/C++ Quickstart

Get up and running with EiplGrader for C and C++ in minutes.

## Prerequisites

- GCC/G++ compiler
- Python 3.7+ (for running EiplGrader)
- OpenAI API key (or compatible LLM API)

## Installation

```bash
pip install eiplgrader
```

## C Example

### 1. Generate C Code

```python
from eiplgrader.codegen import CodeGenerator

# Initialize the code generator for C
api_key = "your-openai-api-key"
generator = CodeGenerator(api_key, language="c")

# Generate code from a student's explanation
result = generator.generate_code(
    student_response="that counts the number of vowels in a string",
    function_name="countVowels",
    gen_type="cgbg"
)

print("Generated C code:")
print(result["code"][0])
```

Output:
```c
int countVowels(char* str) {
    int count = 0;
    for (int i = 0; str[i] != '\0'; i++) {
        char c = tolower(str[i]);
        if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') {
            count++;
        }
    }
    return count;
}
```

### 2. Test the C Code

```python
from eiplgrader.tester import CodeTester

# Define test cases - C REQUIRES explicit type annotations
test_cases = [
    {
        "parameters": {"str": "Hello World"},
        "parameter_types": {"str": "char*"},  # C string type
        "expected": 3,
        "expected_type": "int"
    },
    {
        "parameters": {"str": "AEIOU"},
        "parameter_types": {"str": "char*"},
        "expected": 5,
        "expected_type": "int"
    },
    {
        "parameters": {"str": "xyz"},
        "parameter_types": {"str": "char*"},
        "expected": 0,
        "expected_type": "int"
    }
]

tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name="countVowels",
    language="c"
)

results = tester.run_tests()
print(f"Tests passed: {results.successes}/{results.testsRun}")
```

## C++ Example

### 1. Generate C++ Code

```python
# Initialize for C++
generator = CodeGenerator(api_key, language="cpp")

# Generate code
result = generator.generate_code(
    student_response="that removes duplicates from a vector of integers",
    function_name="removeDuplicates",
    gen_type="cgbg"
)

print("Generated C++ code:")
print(result["code"][0])
```

Output:
```cpp
std::vector<int> removeDuplicates(std::vector<int> nums) {
    std::set<int> seen;
    std::vector<int> result;
    for (int num : nums) {
        if (seen.insert(num).second) {
            result.push_back(num);
        }
    }
    return result;
}
```

### 2. Test the C++ Code

```python
# C++ test cases with STL containers
test_cases = [
    {
        "parameters": {"nums": [1, 2, 2, 3, 3, 3, 4]},
        "parameter_types": {"nums": "std::vector<int>"},
        "expected": [1, 2, 3, 4],
        "expected_type": "std::vector<int>"
    },
    {
        "parameters": {"nums": [5, 5, 5, 5]},
        "parameter_types": {"nums": "std::vector<int>"},
        "expected": [5],
        "expected_type": "std::vector<int>"
    }
]

tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name="removeDuplicates",
    language="cpp"
)
```

## Language-Specific Features

### C Type System

| Generic Type | C Type | Example |
|-------------|--------|---------|
| `int` | `int` | `42` |
| `double` | `double` | `3.14` |
| `string` | `char*` | `"hello"` |
| `bool` | `int` | `1` or `0` |
| `List[int]` | `int*` | Array pointer |

### C++ Type System

| Generic Type | C++ Type | Example |
|-------------|----------|---------|
| `int` | `int` | `42` |
| `double` | `double` | `3.14` |
| `string` | `std::string` | `"hello"` |
| `bool` | `bool` | `true` |
| `List[int]` | `std::vector<int>` | `{1, 2, 3}` |

### Required Headers

The test harness automatically includes necessary headers:

**C:**
- `<stdio.h>`
- `<stdlib.h>`
- `<string.h>`
- `<stdbool.h>`
- `<ctype.h>`

**C++:**
- `<iostream>`
- `<vector>`
- `<string>`
- `<algorithm>`
- `<set>`
- `<map>`

## Common Patterns

### C: Array Operations with Size

```python
# C arrays need explicit size parameters
result = generator.generate_code(
    student_response="that finds the sum of an integer array",
    function_name="arraySum"
)

test_cases = [
    {
        "parameters": {"arr": [1, 2, 3, 4, 5], "size": 5},
        "parameter_types": {"arr": "int*", "size": "int"},
        "expected": 15,
        "expected_type": "int"
    }
]
```

### C: String Manipulation

```python
result = generator.generate_code(
    student_response="that reverses a string in place",
    function_name="reverseString"
)

test_cases = [
    {
        "parameters": {"str": "hello"},
        "parameter_types": {"str": "char*"},
        "expected": "olleh",
        "expected_type": "char*",
        "inplace": "1"  # Modifies string in place
    }
]
```

### C++: STL Containers

```python
result = generator.generate_code(
    student_response="that merges two sorted vectors",
    function_name="mergeSorted"
)

test_cases = [
    {
        "parameters": {
            "vec1": [1, 3, 5],
            "vec2": [2, 4, 6]
        },
        "parameter_types": {
            "vec1": "std::vector<int>",
            "vec2": "std::vector<int>"
        },
        "expected": [1, 2, 3, 4, 5, 6],
        "expected_type": "std::vector<int>"
    }
]
```

### C++: String Operations

```python
result = generator.generate_code(
    student_response="that splits a string by spaces",
    function_name="splitString"
)

test_cases = [
    {
        "parameters": {"str": "hello world test"},
        "parameter_types": {"str": "std::string"},
        "expected": ["hello", "world", "test"],
        "expected_type": "std::vector<std::string>"
    }
]
```

## Memory Management Considerations

### C: Manual Memory

```python
# Be careful with dynamically allocated memory
# The test harness handles basic cases but avoid complex allocations
result = generator.generate_code(
    student_response="that creates an array of fibonacci numbers",
    function_name="fibonacci"
)

# Prefer stack-allocated arrays or pass pre-allocated memory
test_cases = [
    {
        "parameters": {"n": 5, "result": [0, 0, 0, 0, 0]},
        "parameter_types": {"n": "int", "result": "int*"},
        "expected": [0, 1, 1, 2, 3],
        "expected_type": "int*",
        "inplace": "1"  # Fills the provided array
    }
]
```

### C++: RAII and Smart Pointers

C++ code typically uses RAII (vectors, strings) which handles memory automatically:

```python
# C++ STL containers manage their own memory
test_cases = [
    {
        "parameters": {"size": 1000000},
        "parameter_types": {"size": "int"},
        "expected": 1000000,  # Large vector created and destroyed safely
        "expected_type": "int"
    }
]
```

## In-Place Modifications

### C: Pointer-based Modifications

```python
# Mode 1: Modifies through pointer
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5], "size": 5},
    "parameter_types": {"arr": "int*", "size": "int"},
    "expected": [1, 1, 3, 4, 5],
    "expected_type": "int*",
    "inplace": "1"  # Sorts array in place
}
```

### C++: Reference-based Modifications

```python
# C++ can use references for in-place modifications
test_case = {
    "parameters": {"vec": [3, 1, 4, 1, 5]},
    "parameter_types": {"vec": "std::vector<int>"},
    "expected": [1, 1, 3, 4, 5],
    "expected_type": "std::vector<int>",
    "inplace": "1"  # Modifies vector in place
}
```

## Compilation Details

### C Compilation
- Compiler: `gcc`
- Standard: C99 by default
- Flags: `-o output_file input_file.c`

### C++ Compilation
- Compiler: `g++`
- Standard: C++17 (`-std=c++17`)
- Flags: `-std=c++17 -o output_file input_file.cpp`

## Error Handling

Both C and C++ provide compilation and runtime error information:

```python
try:
    results = tester.run_tests()
    if not results.allPassed:
        for failure in results.failures:
            print(f"Test failed: {failure.test}")
            if "Compilation failed" in str(failure.error):
                print("Compilation error - check syntax and types")
            elif "Segmentation fault" in str(failure.error):
                print("Memory access error - check array bounds")
except Exception as e:
    print(f"Error: {e}")
```

## Best Practices

### For C:

1. **Always specify array sizes**:
   ```python
   # C arrays need explicit size
   "parameters": {"arr": [1, 2, 3], "n": 3}
   "parameter_types": {"arr": "int*", "n": "int"}
   ```

2. **Use appropriate string type**:
   ```python
   "parameter_types": {"str": "char*"}  # not "string"
   ```

3. **Handle null termination**:
   ```c
   // Generated code should handle '\0' for strings
   ```

### For C++:

1. **Prefer STL containers**:
   ```python
   "parameter_types": {"nums": "std::vector<int>"}  # not int*
   ```

2. **Use std:: prefix**:
   ```python
   "std::string"  # not just "string"
   "std::vector<int>"  # not "vector<int>"
   ```

3. **Leverage C++ features**:
   ```cpp
   // Range-based for loops
   // auto keyword
   // STL algorithms
   ```

## Common Gotchas

1. **C String Literals**: Test harness handles string allocation
   ```python
   # This works - test harness allocates memory
   "parameters": {"str": "hello"}
   ```

2. **Array Return Types**: C can't return arrays directly
   ```c
   // Use out parameters or dynamic allocation
   void processArray(int* input, int* output, int size);
   ```

3. **C++ Template Errors**: Can be verbose
   ```python
   # Be specific with types to avoid template issues
   "std::vector<int>" not "std::vector"
   ```

## Next Steps

- Explore [Go Quickstart](go.md) for a modern compiled language
- Learn about [Test Case Format](../guide/test-cases.md) for complex types
- See [Language Support](../guide/languages.md) for detailed C/C++ info
- Try [Haskell Quickstart](haskell.md) for functional programming