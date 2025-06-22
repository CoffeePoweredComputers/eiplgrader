---
layout: default
title: Basic Usage
parent: User Guide
nav_order: 1
---

# Basic Usage

Learn the core concepts and workflows for using EiplGrader effectively.

## Overview

EiplGrader follows a simple two-step process:
1. **Generate code** from natural language descriptions using LLMs
2. **Test the code** against predefined test cases

```python
# Step 1: Generate
generator = CodeGenerator(api_key, language="python")
result = generator.generate_code(student_response, function_name)

# Step 2: Test
tester = CodeTester(code=result["code"][0], test_cases, function_name, language="python")
results = tester.run_tests()
```

## Core Components

### CodeGenerator

The `CodeGenerator` class interfaces with LLMs to transform natural language into code.

```python
from eiplgrader.codegen import CodeGenerator

# Initialize with your API key
generator = CodeGenerator(
    api_key="your-api-key",
    client_type="openai"  # Currently supported: "openai"
)
```

### CodeTester

The `CodeTester` class executes generated code against test cases.

```python
from eiplgrader.tester import CodeTester

# Create tester with code and test cases
tester = CodeTester(
    code=generated_code,
    test_cases=test_cases,
    function_name="my_function",
    language="python"
)
```

## Generation Types

### 1. Code Generation Based Grading (CGBG)

The default mode - generates code from natural language descriptions.

```python
result = generator.generate_code(
    student_response="that calculates the factorial of a number",
    function_name="factorial",
    gen_type="cgbg"  # Default
)
```

**Use cases:**
- Grading "explain in plain language" questions
- Converting algorithm descriptions to code
- Testing conceptual understanding

### 2. Function Redefinition (redef)

Generates code from function signatures and assumptions.

```python
result = generator.generate_code(
    student_response="calculate_average",  # Function name
    gen_type="redef",
    params="numbers",  # Parameters
    assumptions="numbers is a list of integers"
)
```

**Use cases:**
- Implementing predefined interfaces
- Generating code from specifications
- Testing implementation skills

## Test Case Format

### For Dynamic Languages (Python, JavaScript)

```python
test_cases = [
    {
        "parameters": {"x": 5, "y": 3},
        "expected": 8
    },
    {
        "parameters": {"x": -1, "y": 1},
        "expected": 0
    }
]
```

### For Static Languages (Java, C++, Go, etc.)

```python
test_cases = [
    {
        "parameters": {"x": 5, "y": 3},
        "parameter_types": {"x": "int", "y": "int"},
        "expected": 8,
        "expected_type": "int"
    }
]
```

## Working with Results

### Generation Results

```python
result = generator.generate_code(...)

# Access generated code
code = result["code"][0]  # First variant
all_variants = result["code"]  # List of all variants

# Check if segmentation was performed
if "segmentation" in result:
    segments = result["segmentation"]
```

### Test Results

```python
results = tester.run_tests()

# Check overall success
if results.allPassed:
    print("All tests passed!")
else:
    print(f"Passed: {results.successes}/{results.testsRun}")

# Access detailed results
for failure in results.failures:
    print(f"Failed: {failure.test}")
    print(f"Expected: {failure.expected}, Got: {failure.actual}")
    if failure.error:
        print(f"Error: {failure.error}")
```

## Common Workflows

### Single Function Testing

```python
# 1. Define the task
task = "that reverses a string"
function_name = "reverse_string"

# 2. Generate code
result = generator.generate_code(task, function_name=function_name)

# 3. Define test cases
test_cases = [
    {"parameters": {"s": "hello"}, "expected": "olleh"},
    {"parameters": {"s": "world"}, "expected": "dlrow"},
    {"parameters": {"s": ""}, "expected": ""}
]

# 4. Test the code
tester = CodeTester(
    code=result["code"][0],
    test_cases=test_cases,
    function_name=function_name,
    language="python"
)

# 5. Evaluate results
results = tester.run_tests()
score = (results.successes / results.testsRun) * 100
print(f"Score: {score}%")
```

### Batch Processing

```python
# Process multiple student responses
student_responses = [
    "that adds two numbers",
    "that multiplies two numbers",
    "that subtracts the second number from the first"
]

results = []
for response in student_responses:
    # Generate code
    gen_result = generator.generate_code(response)
    
    # Test with appropriate test cases
    test_result = tester.run_tests()
    
    results.append({
        "response": response,
        "passed": test_result.allPassed,
        "score": test_result.successes / test_result.testsRun
    })
```

### Multi-Language Support

```python
# Test the same logic in different languages
languages = ["python", "javascript", "java"]
task = "that finds the maximum value in a list"

for language in languages:
    # Initialize generator for specific language
    generator = CodeGenerator(api_key, language=language)
    
    # Generate code
    result = generator.generate_code(task)
    
    # Adjust test cases for language
    if language in ["python", "javascript"]:
        test_cases = [
            {"parameters": {"arr": [1, 5, 3]}, "expected": 5}
        ]
    else:  # Static languages
        test_cases = [
            {
                "parameters": {"arr": [1, 5, 3]},
                "parameter_types": {"arr": "int[]"},
                "expected": 5,
                "expected_type": "int"
            }
        ]
    
    # Test in specific language
    tester = CodeTester(
        code=result["code"][0],
        test_cases=test_cases,
        language=language
    )
```

## Error Handling

### Generation Errors

```python
try:
    result = generator.generate_code(student_response)
except Exception as e:
    print(f"Generation failed: {e}")
    # Handle API errors, invalid responses, etc.
```

### Testing Errors

```python
try:
    results = tester.run_tests()
except Exception as e:
    print(f"Testing failed: {e}")
    # Handle compilation errors, runtime errors, etc.

# Check individual test failures
for failure in results.failures:
    if "Compilation failed" in str(failure.error):
        print("Code has syntax errors")
    elif "timeout" in str(failure.error).lower():
        print("Code execution timed out")
    else:
        print(f"Runtime error: {failure.error}")
```

## Configuration Options

### Generator Options

```python
generator.generate_code(
    student_response="description",
    function_name="my_func",      # Function name to generate
    gen_type="cgbg",             # Generation type
    num_to_gen=1,                # Number of variants
    temperature=1.0,             # LLM temperature
    model="gpt-4o"               # Model to use
)
```

### Tester Options

```python
CodeTester(
    code=code,
    test_cases=test_cases,
    function_name="my_func",
    language="python",
    timeout=30  # Timeout per test in seconds
)
```

## Best Practices

### 1. Clear Descriptions
```python
# Good - specific and unambiguous
"that returns the sum of all even numbers in a list"

# Poor - vague
"that processes numbers"
```

### 2. Comprehensive Test Cases
```python
test_cases = [
    # Normal case
    {"parameters": {"lst": [1, 2, 3, 4]}, "expected": 6},
    # Edge case - empty
    {"parameters": {"lst": []}, "expected": 0},
    # Edge case - no even numbers
    {"parameters": {"lst": [1, 3, 5]}, "expected": 0},
    # Edge case - all even
    {"parameters": {"lst": [2, 4, 6]}, "expected": 12}
]
```

### 3. Language-Appropriate Types
```python
# Python - automatic inference
{"parameters": {"x": 5}}

# Java - explicit types
{
    "parameters": {"x": 5},
    "parameter_types": {"x": "int"},
    "expected_type": "int"
}
```

### 4. Error Recovery
```python
# Try multiple variants if first fails
for i, code_variant in enumerate(result["code"]):
    try:
        tester = CodeTester(code=code_variant, ...)
        results = tester.run_tests()
        if results.allPassed:
            break
    except Exception:
        continue  # Try next variant
```

## Next Steps

- Explore [Advanced Features](advanced-features.md) for more sophisticated usage
- Learn about [Test Case Format](test-cases.md) in detail
- See [Language Support](languages.md) for language-specific features
- Try [Docker Usage](docker.md) for secure execution