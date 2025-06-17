# Java Language Support

This document describes the Java language implementation for eiplgrader.

## Overview

The Java implementation provides support for generating and testing Java code through the eiplgrader system. It handles Java's class-based structure, static methods, instance methods, and manages compilation with `javac` and execution with `java`.

## Components

### JavaAdapter (`eiplgrader.languages.adapters.java_adapter`)

The adapter handles:
- Generating Java-specific prompts for LLMs
- Extracting Java code from LLM responses
- Basic syntax validation
- Support for both CGBG and function redefinition modes

Key features:
- Automatically wraps methods in a `Solution` class
- Handles multiple code generation formats
- Validates class and method definitions

### JavaExecutor (`eiplgrader.languages.executors.java_executor`)

The executor handles:
- Compilation using `javac`
- Execution using `java`
- Test harness generation
- JSON serialization (with Gson fallback to simple implementation)
- Support for different test modes (normal, in-place, both)

## Code Structure

Generated Java code follows this pattern:

```java
public class Solution {
    public static <return_type> functionName(<parameters>) {
        // implementation
    }
}
```

The executor wraps this in a test harness:

```java
import java.util.*;
import com.google.gson.Gson;

class Solution {
    // Generated method(s)
}

public class Test {
    private static String toJson(Object obj) {
        // JSON serialization
    }
    
    public static void main(String[] args) {
        // Parse arguments
        // Call function
        // Output JSON result
    }
}
```

## Test Modes

1. **Normal mode** (`inplace="0"`): Function returns a value
2. **In-place mode** (`inplace="1"`): Function modifies arguments (e.g., arrays)
3. **Both mode** (`inplace="2"`): Function both modifies and returns

## Type Support

The executor automatically infers Java types from Python values:
- `bool` → `boolean`
- `int` → `int`
- `float` → `double`
- `str` → `String`
- `list[int]` → `int[]`
- `list[float]` → `double[]`
- `list[str]` → `String[]`

## Dependencies

### Optional: Gson
The executor attempts to use Google's Gson library for JSON serialization. It searches for Gson in common locations:
- `/usr/share/java/gson.jar`
- `/usr/local/share/java/gson.jar`
- Maven repository (`~/.m2/repository/`)
- Gradle cache (`~/.gradle/caches/`)

If Gson is not available, it falls back to a simple JSON implementation.

## Error Handling

The implementation handles:
- Compilation errors
- Runtime exceptions
- Timeout management
- Missing dependencies (Gson fallback)

## Usage Example

```python
from eiplgrader.languages import language_registry

# Get Java components
java_adapter = language_registry.get_adapter("java")
java_executor = language_registry.get_executor("java")

# Generate code
prompt = java_adapter.generate_prompt(
    student_response="adds two numbers",
    function_name="add"
)

# Extract from LLM response
code = java_adapter.extract_code(llm_response)[0]

# Test the code
result = java_executor.execute_test(code, {
    "function_name": "add",
    "parameters": {"a": 5, "b": 3},
    "expected": 8
})

# Clean up
java_executor.cleanup()
```

## Limitations

1. Currently supports only static methods (instance methods can be added if needed)
2. Limited to basic types and arrays (no custom objects)
3. Package management is simplified (all code in default package)
4. No support for external libraries beyond Gson

## Testing

Run tests with:
```bash
python -m pytest tests/languages/test_java.py -v
```