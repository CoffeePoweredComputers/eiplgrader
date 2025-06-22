"""
Pytest configuration and fixtures for EiplGrader test suite.
"""

import os
import shutil
import sys
import tempfile
from pathlib import Path
from typing import Dict, Any, List

import pytest

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))


@pytest.fixture
def temp_test_dir():
    """Create a temporary directory for tests."""
    temp_dir = tempfile.mkdtemp()
    yield temp_dir
    shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.fixture
def sample_test_cases():
    """Provide sample test cases for different scenarios."""
    return {
        "simple_math": {"parameters": {"a": 5, "b": 3}, "expected": 8},
        "string_processing": {
            "parameters": {"text": "hello world"},
            "expected": "HELLO WORLD",
        },
        "list_processing": {
            "parameters": {"numbers": [1, 2, 3, 4, 5]},
            "expected": [2, 4, 6, 8, 10],
        },
        "edge_case_empty": {"parameters": {"items": []}, "expected": []},
        "boolean_logic": {"parameters": {"condition": True}, "expected": False},
    }


@pytest.fixture
def sample_code_snippets():
    """Provide sample code snippets for different languages."""
    return {
        "python": "def add(a, b):\n    return a + b",
        "javascript": "function add(a, b) {\n    return a + b;\n}",
        "java": "public static int add(int a, int b) {\n    return a + b;\n}",
        "c": "int add(int a, int b) {\n    return a + b;\n}",
        "cpp": "int add(int a, int b) {\n    return a + b;\n}",
        "go": "func add(a, b int) int {\n    return a + b\n}",
        "haskell": "add :: Int -> Int -> Int\nadd a b = a + b",
    }


@pytest.fixture
def malformed_code_snippets():
    """Provide malformed code snippets for error testing."""
    return {
        "python": "def add(a, b\n    return a + b",  # Missing closing parenthesis
        "javascript": "function add(a, b) {\n    return a + b\n",  # Missing closing brace
        "java": "public static int add(int a, int b) {\n    return a + b\n",  # Missing closing brace
        "c": "int add(int a, int b) {\n    return a + b\n",  # Missing closing brace
        "cpp": "int add(int a, int b) {\n    return a + b\n",  # Missing closing brace
        "go": "func add(a, b int) int {\n    return a + b\n",  # Missing closing brace
        "haskell": "add :: Int -> Int -> Int\nadd a b = a +",  # Incomplete expression
    }


@pytest.fixture
def type_inference_languages():
    """List of languages that support type inference."""
    return ["python", "javascript"]


@pytest.fixture
def explicit_type_languages():
    """List of languages that require explicit types."""
    return ["c", "cpp", "java", "haskell"]


@pytest.fixture
def all_supported_languages():
    """List of all supported languages."""
    return ["python", "javascript", "java", "c", "cpp", "go", "haskell"]


# Pytest markers for test categorization
def pytest_configure(config):
    """Configure pytest markers."""
    config.addinivalue_line(
        "markers", "type_system: marks tests related to type system functionality"
    )
    config.addinivalue_line(
        "markers", "infrastructure: marks tests related to infrastructure functionality"
    )
    config.addinivalue_line(
        "markers", "edge_cases: marks tests related to edge cases and error handling"
    )
    config.addinivalue_line("markers", "integration: marks integration tests")
    config.addinivalue_line(
        "markers", "slow: marks tests as slow (deselect with '-m \"not slow\"')"
    )


def pytest_collection_modifyitems(config, items):  # pylint: disable=unused-argument
    """Modify test collection to add markers automatically."""
    for item in items:
        # Add type_system marker
        if "type_system" in item.nodeid or "type" in item.name.lower():
            item.add_marker(pytest.mark.type_system)

        # Add infrastructure marker
        if (
            "infrastructure" in item.nodeid
            or "adapter" in item.name.lower()
            or "registry" in item.name.lower()
        ):
            item.add_marker(pytest.mark.infrastructure)

        # Add edge_cases marker
        if (
            "edge_case" in item.nodeid
            or "error" in item.name.lower()
            or "malformed" in item.name.lower()
        ):
            item.add_marker(pytest.mark.edge_cases)

        # Add integration marker
        if "integration" in item.nodeid or "workflow" in item.name.lower():
            item.add_marker(pytest.mark.integration)


@pytest.fixture
def mock_llm_responses():
    """Mock LLM responses for testing without API calls."""
    return {
        "python": {
            "simple_add": "```python\ndef add(a, b):\n    return a + b\n```",
            "list_sum": "```python\ndef sum_list(numbers):\n    total = 0\n    for num in numbers:\n        total += num\n    return total\n```",
        },
        "javascript": {
            "simple_add": "```javascript\nfunction add(a, b) {\n    return a + b;\n}\n```",
            "list_sum": "```javascript\nfunction sumList(numbers) {\n    let total = 0;\n    for (let num of numbers) {\n        total += num;\n    }\n    return total;\n}\n```",
        },
        "java": {
            "simple_add": "```java\npublic static int add(int a, int b) {\n    return a + b;\n}\n```"
        },
        "c": {"simple_add": "```c\nint add(int a, int b) {\n    return a + b;\n}\n```"},
        "cpp": {
            "simple_add": "```cpp\nint add(int a, int b) {\n    return a + b;\n}\n```"
        },
        "go": {
            "simple_add": "```go\nfunc add(a, b int) int {\n    return a + b\n}\n```"
        },
        "haskell": {
            "simple_add": "```haskell\nadd :: Int -> Int -> Int\nadd a b = a + b\n```"
        },
    }


@pytest.fixture
def comprehensive_test_cases():
    """Comprehensive test cases for cross-language testing."""
    return {
        "type_inference": {
            "parameters": {"a": 10, "b": 20},
            "expected": 30,
            "function_name": "add",
        },
        "explicit_types": {
            "parameters": {"a": 10, "b": 20},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 30,
            "expected_type": "int",
            "function_name": "add",
        },
        "string_types": {
            "parameters": {"text": "hello"},
            "parameter_types": {"text": "string"},
            "expected": "HELLO",
            "expected_type": "string",
            "function_name": "to_upper",
        },
        "list_types": {
            "parameters": {"numbers": [1, 2, 3]},
            "parameter_types": {"numbers": "List[int]"},
            "expected": 6,
            "expected_type": "int",
            "function_name": "sum_list",
        },
    }
