---
layout: default
title: Advanced Features
parent: User Guide
nav_order: 2
---

# Advanced Features

Explore sophisticated capabilities of EiplGrader for complex grading scenarios.

## Multiple Function Variants

Generate multiple implementations of the same function to find the best one.

### Generating Variants

```python
from eiplgrader.codegen import CodeGenerator

generator = CodeGenerator(api_key, language="python")

# Generate 5 different implementations
result = generator.generate_code(
    student_response="that implements binary search on a sorted list",
    function_name="binary_search",
    num_to_gen=5  # Generate 5 variants
)

# Access all variants
for i, code in enumerate(result["code"]):
    print(f"Variant {i + 1}:")
    print(code)
    print("-" * 40)
```

### Testing Multiple Variants

```python
from eiplgrader.tester import CodeTester

# Test cases for binary search
test_cases = [
    {
        "parameters": {"arr": [1, 3, 5, 7, 9], "target": 5},
        "expected": 2  # Index of 5
    },
    {
        "parameters": {"arr": [1, 3, 5, 7, 9], "target": 6},
        "expected": -1  # Not found
    }
]

# Test each variant
best_variant = None
best_score = 0

for i, code in enumerate(result["code"]):
    try:
        tester = CodeTester(
            code=code,
            test_cases=test_cases,
            function_name="binary_search",
            language="python"
        )
        results = tester.run_tests()
        
        score = results.successes / results.testsRun
        print(f"Variant {i + 1}: {score * 100}% tests passed")
        
        if score > best_score:
            best_score = score
            best_variant = i
            
    except Exception as e:
        print(f"Variant {i + 1} failed: {e}")

print(f"\nBest variant: #{best_variant + 1} with {best_score * 100}% success")
```

## Code Segmentation

Map natural language explanations to specific code segments for detailed feedback.

### Setting Up Segmentation

```python
# Create few-shot examples for segmentation
segmentation_examples = [
    {
        "nl_explanation": "First, I check if the list is empty. Then I iterate through the list to find the maximum.",
        "code": """def find_max(lst):
    if not lst:
        return None
    max_val = lst[0]
    for val in lst:
        if val > max_val:
            max_val = val
    return max_val""",
        "segmentation": [
            {"segment": "I check if the list is empty", "lines": [2]},
            {"segment": "I iterate through the list to find the maximum", "lines": [4, 5, 6, 7]}
        ]
    }
]

# Save to JSON file
import json
with open("segmentation_few_shot.json", "w") as f:
    json.dump(segmentation_examples, f, indent=2)
```

### Using Segmentation

```python
# Generate code with segmentation
result = generator.generate_code(
    student_response="""First, I initialize an empty result list. 
                        Then I loop through the input list. 
                        For each element, if it's even, I add it to the result. 
                        Finally, I return the result list.""",
    function_name="filter_even",
    segmentation_few_shot_file="segmentation_few_shot.json"
)

# Access segmentation results
if "segmentation" in result:
    print("Code:")
    print(result["code"][0])
    print("\nSegmentation mapping:")
    for segment in result["segmentation"]:
        print(f"'{segment['segment']}' -> lines {segment['lines']}")
```

### Providing Feedback with Segmentation

```python
def provide_detailed_feedback(code, test_results, segmentation):
    """Provide line-specific feedback based on test failures."""
    
    feedback = []
    
    # Analyze which segments might be problematic
    if not test_results.allPassed:
        # Map errors to code segments
        for failure in test_results.failures:
            if "empty" in str(failure.test).lower():
                # Find segment related to empty list handling
                for seg in segmentation:
                    if "empty" in seg["segment"].lower():
                        feedback.append({
                            "issue": "Empty list handling failed",
                            "segment": seg["segment"],
                            "lines": seg["lines"]
                        })
    
    return feedback

# Example usage
feedback = provide_detailed_feedback(
    result["code"][0], 
    test_results, 
    result["segmentation"]
)

for item in feedback:
    print(f"Issue: {item['issue']}")
    print(f"Related explanation: '{item['segment']}'")
    print(f"Check lines: {item['lines']}")
```

## In-Place Operations

Test functions that modify their arguments rather than returning new values.

### Mode 0: Normal Return (Default)

```python
# Standard function that returns a value
test_case = {
    "parameters": {"numbers": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "0"  # Default - expects return value
}

# Generated function example:
# def sort_list(numbers):
#     return sorted(numbers)
```

### Mode 1: In-Place Modification

```python
# Function modifies the input in place
test_case = {
    "parameters": {"numbers": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "1"  # Tests that 'numbers' is modified
}

# Generated function example:
# def sort_list(numbers):
#     numbers.sort()  # Modifies in place, no return
```

### Mode 2: Modify and Return

```python
# Function both modifies and returns
test_case = {
    "parameters": {"numbers": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],
    "inplace": "2"  # Tests both modification and return
}

# Generated function example:
# def sort_list(numbers):
#     numbers.sort()  # Modifies in place
#     return numbers  # Also returns the modified list
```

### Language-Specific Examples

```python
# Python - List modification
test_cases_python = [
    {
        "parameters": {"lst": [1, 2, 3], "value": 4},
        "expected": [1, 2, 3, 4],
        "inplace": "1"  # append modifies list
    }
]

# Java - Array modification
test_cases_java = [
    {
        "parameters": {"arr": [3, 1, 4], "n": 3},
        "parameter_types": {"arr": "int[]", "n": "int"},
        "expected": [1, 3, 4],
        "expected_type": "int[]",
        "inplace": "1"  # Sort array in place
    }
]

# C++ - Vector modification by reference
test_cases_cpp = [
    {
        "parameters": {"vec": [5, 2, 8, 1]},
        "parameter_types": {"vec": "std::vector<int>"},
        "expected": [1, 2, 5, 8],
        "expected_type": "std::vector<int>",
        "inplace": "1"
    }
]
```

## Custom Timeouts

Configure execution timeouts for long-running or complex functions.

```python
# Set custom timeout per test case
test_cases = [
    {
        "parameters": {"n": 1000000},
        "expected": "result",
        "timeout": 60  # 60 seconds for this test
    },
    {
        "parameters": {"n": 10},
        "expected": "quick_result",
        "timeout": 5   # 5 seconds for this test
    }
]

# Or set timeout for all tests in the tester
tester = CodeTester(
    code=code,
    test_cases=test_cases,
    function_name="process_data",
    language="python"
)
```

## Temperature Control

Adjust the creativity/randomness of generated code.

```python
# Low temperature (0.2) - More deterministic, conventional solutions
conservative_result = generator.generate_code(
    student_response="that sorts a list",
    temperature=0.2
)

# High temperature (1.5) - More creative, varied solutions
creative_result = generator.generate_code(
    student_response="that sorts a list",
    temperature=1.5
)

# Compare different temperature outputs
for temp in [0.0, 0.5, 1.0, 1.5]:
    result = generator.generate_code(
        student_response="that calculates fibonacci numbers",
        temperature=temp
    )
    print(f"Temperature {temp}:")
    print(result["code"][0][:100] + "...")  # First 100 chars
```

## Model Selection

Choose different LLM models for generation.

```python
# Use different models
models = ["gpt-4o", "gpt-4o-turbo", "gpt-3.5-turbo"]

for model in models:
    result = generator.generate_code(
        student_response="that implements quicksort",
        model=model
    )
    print(f"Model {model} generated:")
    print(result["code"][0])
```

## Batch Processing with Parallel Execution

Process multiple students or tasks efficiently.

```python
import concurrent.futures
from typing import List, Dict

def process_student_response(
    generator: CodeGenerator,
    response: str,
    test_cases: List[Dict],
    language: str = "python"
) -> Dict:
    """Process a single student response."""
    try:
        # Generate code
        gen_result = generator.generate_code(response)
        
        # Test code
        tester = CodeTester(
            code=gen_result["code"][0],
            test_cases=test_cases,
            language=language
        )
        test_result = tester.run_tests()
        
        return {
            "response": response,
            "success": True,
            "score": test_result.successes / test_result.testsRun,
            "details": test_result
        }
    except Exception as e:
        return {
            "response": response,
            "success": False,
            "error": str(e)
        }

# Process multiple responses in parallel
student_responses = [
    "that calculates the mean of a list",
    "that finds the median of a list",
    "that computes the mode of a list",
    "that calculates standard deviation"
]

# Define test cases for each task
test_cases_map = {
    "mean": [{"parameters": {"lst": [1, 2, 3, 4, 5]}, "expected": 3.0}],
    "median": [{"parameters": {"lst": [1, 2, 3, 4, 5]}, "expected": 3}],
    "mode": [{"parameters": {"lst": [1, 2, 2, 3]}, "expected": 2}],
    "standard deviation": [{"parameters": {"lst": [2, 4, 6]}, "expected": 2.0}]
}

# Parallel processing
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
    futures = []
    for response in student_responses:
        # Determine which test cases to use
        for key in test_cases_map:
            if key in response.lower():
                test_cases = test_cases_map[key]
                break
        
        future = executor.submit(
            process_student_response,
            generator,
            response,
            test_cases
        )
        futures.append(future)
    
    # Collect results
    results = [future.result() for future in futures]

# Display results
for result in results:
    if result["success"]:
        print(f"✓ {result['response']}: {result['score']*100:.1f}%")
    else:
        print(f"✗ {result['response']}: {result['error']}")
```

## Custom Validation

Add custom validation logic beyond standard test cases.

```python
def validate_code_style(code: str, language: str) -> List[str]:
    """Check code style and return issues."""
    issues = []
    
    if language == "python":
        # Check for PEP 8 compliance
        lines = code.split('\n')
        for i, line in enumerate(lines):
            if len(line) > 79:
                issues.append(f"Line {i+1} exceeds 79 characters")
            if '\t' in line:
                issues.append(f"Line {i+1} uses tabs instead of spaces")
    
    elif language == "java":
        # Check for Java conventions
        if "class Solution" not in code:
            issues.append("Missing Solution class wrapper")
        if not any(line.strip().startswith("import") for line in code.split('\n')):
            issues.append("Consider adding necessary imports")
    
    return issues

# Use custom validation
code = result["code"][0]
style_issues = validate_code_style(code, "python")

if style_issues:
    print("Style issues found:")
    for issue in style_issues:
        print(f"  - {issue}")
```

## Structured Grading Rubrics

Create comprehensive grading rubrics combining multiple criteria.

```python
class GradingRubric:
    def __init__(self):
        self.criteria = []
    
    def add_criterion(self, name: str, weight: float, test_cases: List[Dict]):
        self.criteria.append({
            "name": name,
            "weight": weight,
            "test_cases": test_cases
        })
    
    def grade(self, code: str, function_name: str, language: str) -> Dict:
        total_score = 0
        results = {}
        
        for criterion in self.criteria:
            tester = CodeTester(
                code=code,
                test_cases=criterion["test_cases"],
                function_name=function_name,
                language=language
            )
            
            test_result = tester.run_tests()
            score = (test_result.successes / test_result.testsRun) * criterion["weight"]
            
            results[criterion["name"]] = {
                "score": score,
                "max_score": criterion["weight"],
                "details": test_result
            }
            
            total_score += score
        
        return {
            "total_score": total_score,
            "criteria_results": results
        }

# Example rubric for a sorting function
rubric = GradingRubric()

# Basic functionality (40%)
rubric.add_criterion(
    "basic_functionality",
    40,
    [
        {"parameters": {"lst": [3, 1, 4]}, "expected": [1, 3, 4]},
        {"parameters": {"lst": [1]}, "expected": [1]}
    ]
)

# Edge cases (30%)
rubric.add_criterion(
    "edge_cases",
    30,
    [
        {"parameters": {"lst": []}, "expected": []},
        {"parameters": {"lst": [1, 1, 1]}, "expected": [1, 1, 1]}
    ]
)

# Performance (30%)
rubric.add_criterion(
    "performance",
    30,
    [
        {"parameters": {"lst": list(range(1000, 0, -1))}, "expected": list(range(1, 1001)), "timeout": 1}
    ]
)

# Grade the submission
grade_report = rubric.grade(code, "sort_list", "python")
print(f"Total Score: {grade_report['total_score']}/100")
```

## Next Steps

- Review [Test Case Format](test-cases.md) for complex test scenarios
- Explore [Language Support](languages.md) for language-specific advanced features
- Learn about [Docker Usage](docker.md) for secure execution at scale
- See [Developer Documentation](../developer/) for extending EiplGrader