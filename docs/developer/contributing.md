---
layout: default
title: Contributing
parent: Developer Documentation
nav_order: 6
permalink: /developer/contributing
---

# Contributing to EiplGrader

Thank you for your interest in contributing to EiplGrader! This guide will help you get started with contributing to the project.

## Getting Started

### Prerequisites

- Python 3.8 or higher
- Git
- A GitHub account
- Familiarity with pytest and Python development

### Development Setup

1. **Fork the Repository**
   ```bash
   # Fork on GitHub, then clone your fork
   git clone https://github.com/YOUR_USERNAME/eiplgrader.git
   cd eiplgrader
   ```

2. **Create a Virtual Environment**
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   ```

3. **Install Development Dependencies**
   ```bash
   # Using pip
   pip install -e ".[dev]"
   
   # Or using Poetry if available
   poetry install --with dev
   ```

4. **Set Up Pre-commit Hooks**
   ```bash
   pre-commit install
   ```

5. **Verify Installation**
   ```bash
   # Run tests
   python -m pytest
   
   # Run linting
   ./lint.sh
   ```

## Development Workflow

### 1. Create a Feature Branch

```bash
# Update main branch
git checkout main
git pull upstream main

# Create feature branch
git checkout -b feature/your-feature-name
```

Branch naming conventions:
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation updates
- `refactor/` - Code refactoring
- `test/` - Test additions/modifications

### 2. Make Your Changes

Follow these guidelines:
- Write clear, self-documenting code
- Add docstrings to all public functions/classes
- Follow existing code style
- Add tests for new functionality
- Update documentation as needed

### 3. Test Your Changes

```bash
# Run all tests
python -m pytest

# Run specific tests
python -m pytest tests/unit/test_your_feature.py

# Run with coverage
python -m pytest --cov=eiplgrader

# Run linting
./lint.sh
```

### 4. Commit Your Changes

```bash
# Stage changes
git add .

# Commit with descriptive message
git commit -m "feat: add support for Ruby language"
```

Commit message format:
- `feat:` - New feature
- `fix:` - Bug fix
- `docs:` - Documentation changes
- `style:` - Code style changes
- `refactor:` - Code refactoring
- `test:` - Test changes
- `chore:` - Build/tooling changes

### 5. Push and Create Pull Request

```bash
# Push to your fork
git push origin feature/your-feature-name
```

Then create a pull request on GitHub with:
- Clear title and description
- Link to related issues
- Test results/screenshots if applicable

## Code Standards

### Python Style Guide

We follow PEP 8 with these additions:
- Line length: 88 characters (Black default)
- Use type hints where appropriate
- Prefer f-strings for formatting

Example:
```python
from typing import List, Optional

def calculate_average(numbers: List[float], precision: Optional[int] = 2) -> float:
    """
    Calculate the average of a list of numbers.
    
    Args:
        numbers: List of numbers to average
        precision: Decimal precision for result
        
    Returns:
        The average value rounded to specified precision
        
    Raises:
        ValueError: If numbers list is empty
    """
    if not numbers:
        raise ValueError("Cannot calculate average of empty list")
    
    average = sum(numbers) / len(numbers)
    return round(average, precision)
```

### Documentation Standards

All public APIs must be documented:

```python
class CodeGenerator:
    """
    Generates code from natural language descriptions using LLMs.
    
    This class provides the main interface for transforming student
    responses into executable code implementations.
    
    Attributes:
        api_key: API key for the LLM provider
        language: Target programming language
        temperature: Sampling temperature for generation
        
    Example:
        >>> generator = CodeGenerator(api_key="sk-...")
        >>> result = generator.generate_code(
        ...     student_response="calculate factorial",
        ...     function_name="factorial"
        ... )
    """
```

### Test Standards

Every new feature requires tests:

```python
import pytest
from eiplgrader.your_module import YourClass

class TestYourClass:
    """Test suite for YourClass."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.instance = YourClass()
    
    def test_normal_operation(self):
        """Test normal operation."""
        result = self.instance.method(valid_input)
        assert result == expected_output
    
    def test_edge_case(self):
        """Test edge case handling."""
        with pytest.raises(ValueError):
            self.instance.method(invalid_input)
    
    @pytest.mark.parametrize("input,expected", [
        (1, 1),
        (2, 4),
        (3, 9)
    ])
    def test_multiple_values(self, input, expected):
        """Test with multiple input values."""
        assert self.instance.square(input) == expected
```

## Areas for Contribution

### High Priority

1. **New Language Support**
   - Add support for Rust, Ruby, Swift, etc.
   - See [Adding Languages](languages/adding-languages.html) guide

2. **LLM Provider Integration**
   - Add support for new LLM providers
   - Improve existing integrations

3. **Performance Improvements**
   - Optimize code execution
   - Improve parallel processing
   - Add caching mechanisms

### Medium Priority

1. **Enhanced Error Handling**
   - Better error messages
   - Recovery mechanisms
   - Debugging tools

2. **Documentation**
   - Improve existing docs
   - Add more examples
   - Create tutorials

3. **Test Coverage**
   - Increase test coverage
   - Add edge case tests
   - Performance benchmarks

### Good First Issues

Look for issues labeled `good first issue` on GitHub:
- Simple bug fixes
- Documentation improvements
- Test additions
- Code cleanup

## Pull Request Process

### Before Submitting

- [ ] All tests pass locally
- [ ] Code follows style guidelines
- [ ] Documentation is updated
- [ ] Commit messages follow convention
- [ ] Branch is up to date with main

### PR Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] Performance improvement

## Testing
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Manual testing completed

## Checklist
- [ ] Code follows project style
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] Tests added/updated
```

### Review Process

1. Automated checks run (tests, linting)
2. Code review by maintainers
3. Address feedback
4. Approval and merge

## Development Tools

### Linting and Formatting

```bash
# Format code with Black
black eiplgrader/ tests/

# Check with pylint
pylint eiplgrader/

# Type checking with mypy
mypy eiplgrader/

# All checks
./lint.sh
```

### Debugging

```python
# Use debugger
import pdb; pdb.set_trace()

# Or IPython debugger
import ipdb; ipdb.set_trace()

# VS Code debugging
# Create .vscode/launch.json configuration
```

### Profiling

```python
# Profile code execution
import cProfile
import pstats

profiler = cProfile.Profile()
profiler.enable()

# Your code here

profiler.disable()
stats = pstats.Stats(profiler)
stats.sort_stats('cumulative')
stats.print_stats(10)
```

## Release Process

### Version Numbering

We use semantic versioning (MAJOR.MINOR.PATCH):
- MAJOR: Breaking API changes
- MINOR: New features, backwards compatible
- PATCH: Bug fixes

### Release Steps

1. Update version in `pyproject.toml` or `setup.py`
2. Update CHANGELOG.md
3. Create release PR
4. Tag release after merge
5. Build and publish to PyPI

## Community

### Communication Channels

- **GitHub Issues**: Bug reports, feature requests
- **GitHub Discussions**: General questions, ideas
- **Pull Requests**: Code contributions

### Code of Conduct

We follow the [Contributor Covenant](https://www.contributor-covenant.org/) code of conduct. Please be respectful and inclusive in all interactions.

### Getting Help

- Check existing documentation
- Search closed issues
- Ask in GitHub Discussions
- Tag maintainers in complex issues

## Recognition

Contributors are recognized in:
- CONTRIBUTORS.md file
- Release notes
- Project documentation

## Legal

By contributing, you agree that your contributions will be licensed under the same license as the project (see LICENSE file).

## Quick Reference

### Common Commands

```bash
# Install dev dependencies
pip install -e ".[dev]"

# Run tests
python -m pytest

# Run specific test
python -m pytest tests/unit/test_codegen.py::test_generate_code

# Run linting
./lint.sh

# Format code
black eiplgrader/ tests/

# Type check
mypy eiplgrader/

# Build documentation
cd docs && bundle exec jekyll serve

# Build package
python -m build
```

### Useful Links

- [Issue Tracker](https://github.com/hamiltonfour/eiplgrader/issues)
- [Pull Requests](https://github.com/hamiltonfour/eiplgrader/pulls)
- [Documentation](https://hamiltonfour.github.io/eiplgrader/)
- [PyPI Package](https://pypi.org/project/eiplgrader/)

Thank you for contributing to EiplGrader! 🎉