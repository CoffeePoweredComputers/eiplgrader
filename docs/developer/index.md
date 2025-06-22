---
layout: default
title: Developer Documentation
nav_order: 4
has_children: true
permalink: /developer/
---

# Developer Documentation

Technical documentation for extending and contributing to EiplGrader.

## Documentation Sections

### 🏗️ [Architecture Overview](architecture.html)
Understand the system design and component relationships.
- System architecture diagrams
- Core design principles
- Component interactions
- Data flow patterns

### 🧩 [Core Components](components/)
Deep dive into the main components.
- [CodeGenerator](components/codegen.html) - Natural language to code
- [CodeTester](components/tester.html) - Code execution and testing

### 🌐 [Language System](languages/)
Learn how language support works.
- [Architecture](languages/architecture.html) - Language system design
- [Adding Languages](languages/adding-languages.html) - Step-by-step guide
- [Executors](languages/executors.html) - Execution models

### 📚 [API Reference](api/)
Complete API documentation.
- Class methods and signatures
- Parameter descriptions
- Return value formats
- Usage examples

### 🧪 [Testing](testing.html)
Quality assurance and testing strategies.
- Unit test structure
- Integration testing
- Language-specific tests
- Performance benchmarks

### 🤝 [Contributing](contributing.html)
How to contribute to EiplGrader.
- Development setup
- Code style guidelines
- Pull request process
- Release procedures

## Architecture at a Glance

```
┌─────────────────┐     ┌─────────────────┐
│  CodeGenerator  │     │   CodeTester    │
└────────┬────────┘     └────────┬────────┘
         │                       │
         ▼                       ▼
┌─────────────────┐     ┌─────────────────┐
│Language Adapters│     │Language Executors│
└────────┬────────┘     └────────┬────────┘
         │                       │
         └───────────┬───────────┘
                     ▼
            ┌─────────────────┐
            │Language Registry│
            └─────────────────┘
```

## Key Concepts for Developers

### Separation of Concerns
- **Adapters**: Handle language-specific code generation
- **Executors**: Handle language-specific code execution
- **Registry**: Central registration and discovery

### Extension Points
1. **New Languages**: Add adapter + executor + register
2. **New LLM Providers**: Implement ModelRequest interface
3. **Custom Test Runners**: Extend CodeTester class
4. **New Generation Types**: Extend prompt templates

### Type System
- **Dynamic Languages**: Automatic type inference
- **Static Languages**: Explicit type validation
- **Type Mappers**: Generic to language-specific types

## Development Workflow

### Setting Up
```bash
# Clone repository
git clone https://github.com/hamiltonfour/eiplgrader.git
cd eiplgrader

# Install development dependencies
pip install -e ".[dev]"

# Run tests
python -m pytest

# Run linting
./lint.sh
```

### Adding a Feature
1. Create feature branch
2. Implement with tests
3. Update documentation
4. Submit pull request

### Testing Changes
```bash
# Run unit tests
python -m pytest tests/unit/

# Run integration tests
python -m pytest tests/integration/

# Run specific language tests
python -m pytest tests/ -k "python"
```

## Performance Considerations

### Optimization Points
- Parallel test execution
- Executor pooling and reuse
- Compilation caching
- LLM response caching

### Resource Management
- Temporary file cleanup
- Process isolation
- Memory limits
- Timeout enforcement

## Security Model

### Code Execution Isolation
- Subprocess execution
- Filesystem isolation
- Network restrictions
- Resource limits

### Best Practices
- Never execute in main process
- Always validate inputs
- Clean up temporary files
- Use Docker for production

## Getting Started

New to the codebase? Follow this path:

1. Read [Architecture Overview](architecture.html)
2. Explore [Core Components](components/)
3. Understand [Language System](languages/architecture.html)
4. Set up [Development Environment](contributing.html#setup)

Ready to contribute? Check out our [open issues](https://github.com/hamiltonfour/eiplgrader/issues) or propose a new feature!