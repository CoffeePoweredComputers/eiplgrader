---
layout: default
title: Language Support
parent: User Guide
nav_order: 4
---

# Language Support

Comprehensive guide to language capabilities, requirements, and features in EiplGrader.

## Language Feature Matrix

| Language | Type System | Type Inference | JSON Support | Compilation | Test Execution | Special Features |
|----------|-------------|----------------|--------------|-------------|----------------|------------------|
| **Python** | Dynamic | ✅ Automatic | ✅ Native | ❌ Interpreted | Direct import | AST-based comment removal |
| **JavaScript** | Dynamic | ✅ Automatic | ✅ Native | ❌ Interpreted | Node.js subprocess | Async/Promise support |
| **Go** | Static | ❌ Required | ✅ Via encoding/json | ✅ go build | Embedded values | Import management |
| **Java** | Static | ❌ Required | ❌ Embedded values | ✅ javac | Class-based execution | Solution class wrapping |
| **C++** | Static | ❌ Required | ❌ Embedded values | ✅ g++ -std=c++17 | Direct execution | STL container support |
| **C** | Static | ❌ Required | ❌ Embedded values | ✅ gcc | Direct execution | Pointer type handling |
| **Haskell** | Static | ❌ Required | ❌ Embedded values | ✅ ghc | Module-based | Lazy evaluation support |

## Detailed Language Profiles

### Python
{: .text-purple-000 }

**Executor Type**: `InterpretedLanguageExecutor`

#### Supported Features
- ✅ Direct module import and function execution
- ✅ In-place modifications (all modes: 0, 1, 2)
- ✅ Native JSON serialization/deserialization
- ✅ AST-based code normalization (removes comments)
- ✅ String escape sequence processing
- ✅ Deep copy support for in-place operations
- ✅ Rich error messages with traceback

#### Type Inference
Python provides full automatic type inference from values:

```python
# Types are automatically inferred:
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

#### Test Execution
- Creates temporary module file
- Imports module using importlib
- Calls function directly with parameters
- Handles deep copying for in-place modes

#### Error Handling
- **Structural Errors**: Missing functions, import failures
- **Runtime Errors**: Type errors, value errors, exceptions
- **Timeout Handling**: Configurable timeout (default 30s)

---

### JavaScript
{: .text-purple-000 }

**Executor Type**: `InterpretedLanguageExecutor`

#### Supported Features
- ✅ Node.js runtime execution
- ✅ Async/await function support
- ✅ In-place modifications (all modes: 0, 1, 2)
- ✅ Native JSON support via JSON.stringify/parse
- ✅ Deep cloning for in-place operations
- ✅ Promise handling in test harness
- ✅ Modern ES6+ syntax support

#### Type Inference
JavaScript provides full automatic type inference:

```javascript
// Types are automatically detected:
test_case = {
    "parameters": {
        "count": 42,                    // number
        "price": 19.99,                 // number
        "name": "Product",              // string
        "items": [1, 2, 3],            // array
        "active": true,                 // boolean
        "data": {"key": "value"}       // object
    }
}
```

#### Test Execution
- Generates self-contained test script
- Wraps all functions as potentially async
- Handles promises automatically
- Uses deep cloning for in-place operations

#### Special Considerations
- All functions treated as potentially async
- Deep equality checking for objects/arrays
- Node.js specific error stack traces

---

### Go
{: .text-yellow-000 }

**Executor Type**: `CompiledLanguageExecutor`

#### Important Note
Despite Go having type inference in the language, EiplGrader **requires explicit type annotations** for all test cases.

#### Supported Features
- ✅ Embedded test values in generated main function
- ✅ Automatic import management (fmt, encoding/json, os)
- ✅ Package declaration handling
- ✅ JSON output for complex types (slices, maps)
- ✅ Direct output for simple types
- ✅ In-place modifications (all modes: 0, 1, 2)
- ✅ Slice and map operations

#### Type Requirements
```go
// REQUIRED - Must specify all types explicitly:
test_case = {
    "parameters": {"nums": [1, 2, 3]},
    "parameter_types": {"nums": "[]int"},    // Required!
    "expected": 6,
    "expected_type": "int"                   // Required!
}
```

#### Type Mappings
| Generic Type | Go Type | Example |
|-------------|---------|---------|
| `int` | `int` | `42` |
| `double` | `float64` | `3.14` |
| `string` | `string` | `"hello"` |
| `bool` | `bool` | `true` |
| `List[T]` | `[]T` | `[]int{1, 2, 3}` |

#### Compilation & Execution
- **Compiler**: `go build`
- **Package**: Always uses `package main`
- **Imports**: Automatically managed based on usage
- **Output**: JSON for complex types, fmt for simple types

---

### Java
{: .text-yellow-000 }

**Executor Type**: `CompiledLanguageExecutor`

#### Supported Features
- ✅ Solution class wrapping
- ✅ Automatic import management (java.util.*)
- ✅ Parameter embedding in main method
- ✅ Type-specific output formatting
- ✅ Array initialization and printing
- ✅ In-place modifications (all modes: 0, 1, 2)
- ✅ Static and instance method support

#### Type Requirements
```java
// REQUIRED - Must specify all types explicitly:
test_case = {
    "parameters": {"name": "Alice", "age": 25},
    "parameter_types": {"name": "String", "age": "int"},  // Required!
    "expected": "Alice is 25",
    "expected_type": "String"                              // Required!
}
```

#### Type Mappings
| Generic Type | Java Type | Notes |
|-------------|-----------|-------|
| `int` | `int` | Primitive preferred |
| `double` | `double` | Primitive preferred |
| `string` | `String` | **Capital S** |
| `bool` | `boolean` | Not `Boolean` |
| `List[T]` | `T[]` | Arrays preferred |

#### Special Handling
- Code wrapped in Solution class if not present
- Handles both static and instance methods
- Custom array printing with Arrays.toString()
- Automatic import extraction and management

#### Compilation & Execution
- **Compiler**: `javac`
- **Runtime**: `java`
- **Class Structure**: `Test` class with `main` method
- **Output**: System.out.println with type-specific formatting

---

### C++
{: .text-yellow-000 }

**Executor Type**: `CompiledLanguageExecutor`

#### Supported Features
- ✅ STL container support (vector, string, set, map)
- ✅ Header management (automatic inclusion)
- ✅ Template function support
- ✅ Custom output formatting for containers
- ✅ In-place modifications via references/pointers
- ✅ Modern C++17 features
- ✅ RAII and automatic memory management

#### Type Requirements
```cpp
// REQUIRED - Must specify all types explicitly:
test_case = {
    "parameters": {"nums": [1, 2, 3]},
    "parameter_types": {"nums": "std::vector<int>"},  // Required!
    "expected": [3, 2, 1],
    "expected_type": "std::vector<int>"               // Required!
}
```

#### Type Mappings
| Generic Type | C++ Type | Example |
|-------------|----------|---------|
| `int` | `int` | `42` |
| `double` | `double` | `3.14` |
| `string` | `std::string` | `"hello"` |
| `bool` | `bool` | `true` |
| `List[T]` | `std::vector<T>` | `{1, 2, 3}` |

#### Included Headers
- `<iostream>`
- `<vector>`
- `<string>`
- `<algorithm>`
- `<set>`
- `<map>`

#### Compilation & Execution
- **Compiler**: `g++ -std=c++17`
- **Features**: Full C++17 support
- **Memory**: RAII handles cleanup automatically

---

### C
{: .text-yellow-000 }

**Executor Type**: `CompiledLanguageExecutor`

#### Supported Features
- ✅ Pointer-based array handling
- ✅ Manual memory management considerations
- ✅ Custom array printing functions
- ✅ String handling via char*
- ✅ In-place modifications via pointers
- ✅ Standard library functions

#### Type Requirements
```c
// REQUIRED - Must specify all types explicitly:
test_case = {
    "parameters": {"arr": [1, 2, 3], "size": 3},
    "parameter_types": {"arr": "int*", "size": "int"},  // Required!
    "expected": 6,
    "expected_type": "int"                               // Required!
}
```

#### Type Mappings
| Generic Type | C Type | Notes |
|-------------|--------|-------|
| `int` | `int` | Standard integer |
| `double` | `double` | Floating point |
| `string` | `char*` | Null-terminated |
| `bool` | `int` | 0 or 1 |
| `List[T]` | `T*` | Pointer + size param |

#### Special Considerations
- Arrays require separate size parameter
- No built-in container types
- String literals handled by test harness
- Manual memory management (test harness handles basic cases)

#### Included Headers
- `<stdio.h>`
- `<stdlib.h>`
- `<string.h>`
- `<stdbool.h>`
- `<ctype.h>`

#### Compilation & Execution
- **Compiler**: `gcc`
- **Standard**: C99 by default
- **Arrays**: Always passed as pointers with size

---

### Haskell
{: .text-yellow-000 }

**Executor Type**: `CompiledLanguageExecutor`

#### Supported Features
- ✅ Pure functional programming
- ✅ List comprehensions
- ✅ Pattern matching
- ✅ Lazy evaluation
- ✅ Module-based code organization
- ✅ Custom show implementations for output
- ✅ Automatic function name detection

#### Type Requirements
```haskell
-- REQUIRED - Must specify all types explicitly:
test_case = {
    "parameters": {"xs": [1, 2, 3]},
    "parameter_types": {"xs": "[Int]"},  // Required!
    "expected": 6,
    "expected_type": "Int"               // Required!
}
```

#### Type Mappings
| Generic Type | Haskell Type | Example |
|-------------|--------------|---------|
| `int` | `Int` | `42` |
| `double` | `Double` | `3.14` |
| `string` | `String` | `"hello"` |
| `bool` | `Bool` | `True` |
| `List[T]` | `[T]` | `[1, 2, 3]` |

#### Special Features
- Type signatures included in generated code
- Pattern matching support
- Guards and list comprehensions
- Handles infinite lists (with care)
- Module structure: `module Main where`

#### Compilation & Execution
- **Compiler**: `ghc`
- **Module**: Always creates `module Main`
- **Entry**: `main :: IO ()` added automatically
- **Output**: Custom show implementations

---

## Type System Categories

### Languages with Type Inference (Dynamic)
{: .text-green-000 }

**Python and JavaScript** automatically infer types from test case values:

```python
# No type annotations needed:
test_case = {
    "parameters": {"x": 5, "y": "hello"},
    "expected": "5 hello"
}
# Types inferred as: x=int, y=str, result=str
```

### Languages without Type Inference (Static)
{: .text-red-000 }

**Go, Java, C++, C, and Haskell** require explicit type annotations:

```python
# Type annotations REQUIRED:
test_case = {
    "parameters": {"x": 5, "y": "hello"},
    "parameter_types": {"x": "int", "y": "string"},  # Required!
    "expected": "5 hello",
    "expected_type": "string"                         # Required!
}
```

---

## Test Case Requirements by Language

### Simplified Format (Python, JavaScript)
```python
{
    "parameters": {"x": 5, "y": [1, 2, 3]},
    "expected": 8,
    "inplace": "0",          # Optional
    "function_name": "foo",   # Optional
    "timeout": 30            # Optional
}
```

### Explicit Format (Go, Java, C++, C, Haskell)
```python
{
    "parameters": {"x": 5, "y": [1, 2, 3]},
    "parameter_types": {"x": "int", "y": "int[]"},  # REQUIRED
    "expected": 8,
    "expected_type": "int",                          # REQUIRED
    "inplace": "0",                                  # Optional
    "function_name": "foo",                          # Optional
    "timeout": 30                                    # Optional
}
```

---

## In-Place Modification Support

All languages support three in-place modes:

### Mode 0: Normal Return (Default)
Function returns a value, no modifications to parameters.

### Mode 1: In-Place Modification
Function modifies the first parameter in place:
- **Python/JS**: Deep copy of first argument
- **Go/Java**: Modifies slice/array in place
- **C/C++**: Modifies through pointer/reference
- **Haskell**: Not typically used (pure functions)

### Mode 2: Modify and Return
Function both modifies first parameter and returns a value.

---

## Error Handling by Language

### Interpreted Languages (Python, JavaScript)
- Rich error messages with stack traces
- Clear distinction between structural and runtime errors
- Async error handling for JavaScript

### Compiled Languages (Go, Java, C++, C, Haskell)
- Compilation errors with line numbers
- Type mismatch errors during compilation
- Runtime errors include stack traces (where available)
- Language-specific error messages

---

## Performance Considerations

| Language | Startup Time | Execution Speed | Memory Usage |
|----------|--------------|-----------------|--------------|
| Python | Fast | Moderate | Moderate |
| JavaScript | Fast | Moderate | Moderate |
| Go | Moderate (compile) | Fast | Low |
| Java | Slow (compile+JVM) | Fast | High |
| C++ | Moderate (compile) | Very Fast | Low |
| C | Fast (compile) | Very Fast | Very Low |
| Haskell | Slow (compile) | Fast | Moderate |

---

## Best Practices by Language

### Dynamic Languages (Python, JavaScript)
1. Leverage type inference - don't specify types unless needed
2. Use native data structures (lists, dicts, objects)
3. Take advantage of built-in functions
4. Handle edge cases gracefully

### Static Languages (Go, Java, C++, C, Haskell)
1. Always provide complete type information
2. Use language-specific type syntax exactly
3. Consider memory management (C/C++)
4. Follow idiomatic patterns for each language

---

## Common Pitfalls

### Type Annotation Errors
```python
# Wrong - using generic types for Java
{"parameter_types": {"name": "string"}}  # ❌

# Correct - using Java-specific type
{"parameter_types": {"name": "String"}}  # ✅
```

### Array/List Confusion
```python
# Python - uses lists
{"parameters": {"items": [1, 2, 3]}}  # Becomes list

# Java - becomes array
{"parameter_types": {"items": "int[]"}}  # Not ArrayList
```

### Missing Size Parameters (C)
```c
// C arrays need size
{"parameters": {"arr": [1, 2, 3], "n": 3}}  // ✅
{"parameters": {"arr": [1, 2, 3]}}           // ❌
```

---

## Language Selection Guide

Choose a language based on:

1. **Type System Requirements**
   - Need type inference? → Python, JavaScript
   - Strict typing? → Go, Java, C++, C, Haskell

2. **Performance Requirements**
   - Maximum speed? → C, C++
   - Fast enough? → Go, Java
   - Convenience over speed? → Python, JavaScript

3. **Programming Paradigm**
   - Functional? → Haskell (or JavaScript)
   - Object-oriented? → Java, C++, Python
   - Procedural? → C, Go
   - Multi-paradigm? → Python, JavaScript, C++

4. **Student Familiarity**
   - Beginners? → Python
   - Web developers? → JavaScript
   - Systems programmers? → C, C++, Go
   - Enterprise developers? → Java
   - Functional programmers? → Haskell