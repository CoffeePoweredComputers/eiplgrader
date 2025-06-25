---
layout: default
title: Architecture Overview
parent: Developer Documentation
nav_order: 1
mermaid: true
---

# Architecture Overview

Understanding the design and architecture of EiplGrader.

## System Architecture

![System Architecture](/assets/diagrams/architecture_diagram_1_corrected.svg)

## Core Design Principles

### 1. Separation of Concerns

The architecture strictly separates:
- **Code Generation** - Natural language to code transformation
- **Code Testing** - Execution and validation of generated code
- **Language Support** - Language-specific implementation details

### 2. Pluggable Architecture

![Pluggable Architecture](/assets/diagrams/architecture_diagram_2_corrected.svg)

### 3. Type System Flexibility

The system supports both:
- **Dynamic typing** with automatic inference (Python, JavaScript)
- **Static typing** with explicit annotations (Java, C++, Go, etc.)

## Component Details

### CodeGenerator

**Purpose**: Transform natural language descriptions into executable code using LLMs.

**Key Responsibilities**:
- LLM API communication
- Prompt engineering
- Multi-variant generation
- Code segmentation support

**Class Structure**:
```python
class CodeGenerator:
    def __init__(self, api_key: str, client_type: str = "openai", language: str = "python")
    def generate_code(self, student_response: str, **kwargs) -> Dict[str, Any]
    def _create_prompt(self, student_response: str, **kwargs) -> str
    def _parse_response(self, llm_response: str) -> Dict[str, Any]
```

### CodeTester

**Purpose**: Execute generated code against test cases and report results.

**Key Responsibilities**:
- Test case validation
- Language-specific execution
- Result aggregation
- Error handling

**Class Structure**:
```python
class CodeTester:
    def __init__(self, code: str, test_cases: List[Dict], function_name: str, language: str)
    def run_tests(self) -> Union[CodeTestResult, List[CodeTestResult]]
    def _validate_test_cases(self) -> None
    def _run_test(self, code: str) -> CodeTestResult
```

### Language System

The language system consists of three main components:

#### 1. Language Adapters

**Purpose**: Handle language-specific code generation requirements.

**Interface**:
```python
class LanguageAdapter(ABC):
    @abstractmethod
    def get_config(self) -> LanguageConfig
    
    @abstractmethod
    def generate_prompt(self, student_response: str, function_name: str, 
                       gen_type: str, **kwargs) -> str
    
    @abstractmethod
    def extract_code(self, llm_response: str) -> List[str]
    
    @abstractmethod
    def normalize_code(self, code: str) -> str
```

#### 2. Language Executors

**Purpose**: Handle language-specific code execution and testing.

**Interface**:
```python
class LanguageExecutor(ABC):
    @abstractmethod
    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str
    
    @abstractmethod
    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]
    
    @abstractmethod
    def cleanup(self) -> None
```

#### 3. Language Registry

**Purpose**: Central registration and discovery of language support.

```python
class LanguageRegistry:
    def register(self, name: str, adapter: Type[LanguageAdapter]) -> None
                         executor: Type[LanguageExecutor]) -> None
    def get_adapter(self, language: str) -> LanguageAdapter
    def get_executor(self, language: str) -> LanguageExecutor
    def list_languages(self) -> List[str]
```

## Data Flow

### Code Generation Flow

```mermaid
sequenceDiagram
    participant User
    participant CodeGenerator
    participant LanguageAdapter
    participant LLM API
    
    User->>CodeGenerator: generate_code(response, language)
    CodeGenerator->>LanguageAdapter: generate_prompt(response)
    LanguageAdapter-->>CodeGenerator: prompt
    CodeGenerator->>LLM API: send_request(prompt)
    LLM API-->>CodeGenerator: llm_response
    CodeGenerator->>LanguageAdapter: extract_code(llm_response)
    LanguageAdapter-->>CodeGenerator: code_blocks
    CodeGenerator->>LanguageAdapter: normalize_code(code)
    LanguageAdapter-->>CodeGenerator: normalized_code
    CodeGenerator-->>User: code: [...], segmentation: ..
```

### Code Testing Flow

```mermaid
sequenceDiagram
    participant User
    participant CodeTester
    participant LanguageExecutor
    participant FileSystem
    participant Process
    
    User->>CodeTester: run_tests()
    loop For each test case
        CodeTester->>LanguageExecutor: execute_test(code, test_case)
        LanguageExecutor->>LanguageExecutor: prepare_code()
        LanguageExecutor->>FileSystem: write_temp_file()
        alt Compiled Language
            LanguageExecutor->>Process: compile()
            Process-->>LanguageExecutor: binary
        end
        LanguageExecutor->>Process: execute()
        Process-->>LanguageExecutor: output
        LanguageExecutor->>LanguageExecutor: normalize_output()
        LanguageExecutor-->>CodeTester: test_result
    end
    CodeTester->>LanguageExecutor: cleanup()
    CodeTester-->>User: CodeTestResult
```

## Type System Architecture

### Type Inference Pipeline (Dynamic Languages)

```mermaid
graph TB
    TV[Test Value<br/>e.g., 42]
    TI[Type Inferrer]
    TS[Type String<br/>e.g., "int"]
    TC[Test Case<br/>with Types]
    
    TV --> TI
    TI --> TS
    TS --> TC
    
    subgraph "Inference Rules"
        R1[isinstance int → "int"]
        R2[isinstance float → "double"]
        R3[isinstance str → "string"]
        R4[isinstance list → "List[T]"]
    end
    
    TI --> R1
    TI --> R2
    TI --> R3
    TI --> R4
```

### Type Validation Pipeline (Static Languages)

```mermaid
graph TB
    TC[Test Case]
    TV[Type Validator]
    VR[Validation Result]
    
    TC --> TV
    TV --> VR
    
    subgraph "Validation Checks"
        C1[parameter_types present?]
        C2[expected_type present?]
        C3[All params have types?]
        C4[Types valid for language?]
    end
    
    TV --> C1
    TV --> C2
    TV --> C3
    TV --> C4
    
    VR --> |Pass| Execute
    VR --> |Fail| Error[Type Error]
```

## Execution Models

### Interpreted Language Execution

```python
# Python/JavaScript execution model
class InterpretedLanguageExecutor:
    def execute_test(self, code, test_case):
        # 1. Infer/validate types
        test_case = self.validate_or_infer_types(test_case)
        
        # 2. Prepare code with test harness
        prepared_code = self.prepare_code(code, test_case)
        
        # 3. Write to temporary file
        temp_file = self.write_temp_file(prepared_code)
        
        # 4. Execute with interpreter
        result = subprocess.run([self.interpreter, temp_file])
        
        # 5. Parse output
        return self.parse_result(result.stdout)
```

### Compiled Language Execution

```python
# Java/C++/Go execution model
class CompiledLanguageExecutor:
    def execute_test(self, code, test_case):
        # 1. Validate types (required)
        self.validate_types_provided(test_case)
        
        # 2. Prepare code with embedded test values
        prepared_code = self.prepare_code(code, test_case)
        
        # 3. Write and compile
        source_file = self.write_source_file(prepared_code)
        binary = self.compile(source_file)
        
        # 4. Execute binary
        result = subprocess.run([binary])
        
        # 5. Parse output
        return self.parse_result(result.stdout)
```

## Error Handling Strategy

### Error Hierarchy

```mermaid
graph TB
    BE[BaseError]
    GE[GenerationError]
    TE[TestingError]
    CE[ConfigurationError]
    
    SE[StructuralError]
    RE[RuntimeError]
    CME[CompilationError]
    TOE[TimeoutError]
    
    BE --> GE
    BE --> TE
    BE --> CE
    
    TE --> SE
    TE --> RE
    TE --> CME
    TE --> TOE
```

### Error Propagation

1. **Generation Phase**: Errors are caught and wrapped with context
2. **Testing Phase**: Errors are categorized (structural vs runtime)
3. **Result Phase**: Errors are included in test results with details

## Security Considerations

### Code Execution Isolation

```mermaid
graph TB
    subgraph "Security Layers"
        UC[User Code]
        TH[Test Harness]
        TMP[Temp Directory]
        PROC[Subprocess]
        OS[OS Process Isolation]
    end
    
    UC --> TH
    TH --> TMP
    TMP --> PROC
    PROC --> OS
    
    subgraph "Security Features"
        TO[Timeout Limits]
        MEM[Memory Limits]
        FS[Filesystem Isolation]
        NET[Network Isolation]
    end
    
    PROC --> TO
    PROC --> MEM
    OS --> FS
    OS --> NET
```

### Security Best Practices

1. **Subprocess Isolation**: All code runs in separate processes
2. **Timeout Enforcement**: Configurable execution timeouts
3. **Temporary Files**: Cleaned up after execution
4. **No Network Access**: Generated code has no network capabilities


## Performance Considerations

### Optimization Points

1. **Parallel Test Execution**: Tests can run concurrently
2. **Executor Reuse**: Executors maintain state for efficiency
3. **Compilation Caching**: Compiled binaries can be cached
4. **LLM Response Caching**: Identical prompts can use cached responses

### Resource Management

```python
# Resource lifecycle
with CodeTester(code, test_cases) as tester:
    results = tester.run_tests()
    # Automatic cleanup on exit
```

## Extension Points

### Adding a New Language

1. Create adapter class inheriting from `LanguageAdapter`
2. Create executor class inheriting from appropriate base
3. Register with `LanguageRegistry`
4. Add tests and documentation

### Adding a New LLM Provider

1. Create provider class implementing `ModelRequest` interface
2. Add to `CodeGenerator` client types
3. Implement prompt formatting and response parsing

### Custom Test Runners

1. Extend `CodeTester` class
2. Override `_execute_single_test` method
3. Add custom result processing

## Future Architecture Considerations

### Planned Enhancements

1. **Distributed Execution**: Run tests across multiple machines
2. **Result Caching**: Cache test results for identical code
3. **Incremental Testing**: Only rerun affected tests
4. **Plugin System**: Dynamic loading of language support

### Scalability Patterns

```mermaid
graph TB
    subgraph "Current"
        S1[Single Process]
        S2[Sequential Tests]
    end
    
    subgraph "Future"
        M1[Multi-Process]
        M2[Parallel Tests]
        M3[Distributed Workers]
        M4[Result Cache]
    end
    
    S1 --> M1
    S2 --> M2
    M1 --> M3
    M2 --> M4
```

## Next Steps

- Explore [Core Components](components/) for detailed implementation
- Learn about [Language System](languages/) for extension details
- See [API Reference](api/) for complete method documentation
- Review [Testing](testing.md) for quality assurance practices
