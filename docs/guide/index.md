---
layout: default
title: User Guide
nav_order: 3
has_children: true
permalink: /guide/
---

# User Guide

Comprehensive guide to using EiplGrader effectively for grading "Explain in Plain Language" questions.

## Guide Sections

### 📖 [Basic Usage](basic-usage.html)
Learn the core concepts and workflows for using EiplGrader.
- Understanding the two-step process
- Working with CodeGenerator and CodeTester
- Generation types (CGBG and redef)
- Common workflows and patterns

### 🚀 [Advanced Features](advanced-features.html)
Explore sophisticated capabilities for complex grading scenarios.
- Multiple function variants
- Code segmentation for detailed feedback
- In-place operation testing
- Performance optimization

### 📝 [Test Case Format](test-cases.html)
Master the test case format for all supported languages.
- Dynamic language format (Python, JavaScript)
- Static language format (Java, C++, Go, etc.)
- Type inference vs explicit types
- Complex test scenarios

### 🌍 [Language Support](languages.html)
Detailed information about each supported language.
- Complete feature matrix
- Language-specific capabilities
- Type system requirements
- Performance characteristics

### 🐳 [Docker Deployment](docker.html)
Run EiplGrader securely in containerized environments.
- Security hardening and isolation
- Production deployment patterns
- Batch processing strategies
- Kubernetes and orchestration

### ⚠️ [Error Handling](errors.html)
Understand and resolve common errors.
- Error categories and solutions
- Language-specific issues
- Debugging strategies
- Best practices

## Learning Path

### For Beginners
1. Start with [Basic Usage](basic-usage.html)
2. Review [Test Case Format](test-cases.html)
3. Explore your language in [Language Support](languages.html)

### For Advanced Users
1. Dive into [Advanced Features](advanced-features.html)
2. Implement [Docker Deployment](docker.html) for scale
3. Master [Error Handling](errors.html) patterns

### For System Administrators
1. Set up [Docker Deployment](docker.html)
2. Review security in [Error Handling](errors.html)
3. Understand scaling options

## Key Concepts

### Generation Types
- **CGBG**: Code Generation Based Grading - from natural language
- **Redef**: Function Redefinition - from specifications

### Test Modes
- **Mode 0**: Normal return value testing (default)
- **Mode 1**: In-place modification testing
- **Mode 2**: Both modification and return testing

### Type Systems
- **Dynamic**: Python, JavaScript (automatic type inference)
- **Static**: Java, C++, C, Go, Haskell (explicit types required)

## Best Practices

1. **Clear Descriptions**: Provide specific, unambiguous natural language descriptions
2. **Comprehensive Testing**: Include edge cases and boundary conditions
3. **Appropriate Types**: Use language-appropriate type annotations
4. **Error Recovery**: Implement retry strategies for robustness

Ready to dive in? Start with [Basic Usage](basic-usage.html) or jump to any section that interests you!