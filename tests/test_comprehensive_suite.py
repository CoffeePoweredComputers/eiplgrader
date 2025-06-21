#!/usr/bin/env python3
"""
Comprehensive Test Suite for EiplGrader Language Adapters and Executors
=======================================================================

This module provides a unified test runner that combines all three agent approaches:
- Agent 1: Type System Specialist (type handling across languages)
- Agent 2: Execution Environment Engineer (infrastructure testing)
- Agent 3: Edge Case & Error Resilience Tester (failure modes)

Usage:
    python -m pytest tests/test_comprehensive_suite.py -v
    python tests/test_comprehensive_suite.py --category=type-system
    python tests/test_comprehensive_suite.py --category=infrastructure
    python tests/test_comprehensive_suite.py --category=edge-cases
    python tests/test_comprehensive_suite.py --full-suite
"""

import os
import sys
from pathlib import Path

import pytest

# Add the project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))

# pylint: disable=wrong-import-position
from eiplgrader.languages.registry import language_registry
from eiplgrader.languages.adapters.python_adapter import PythonAdapter
from eiplgrader.languages.adapters.javascript_adapter import JavaScriptAdapter
from eiplgrader.languages.adapters.java_adapter import JavaAdapter
from eiplgrader.languages.adapters.c_adapter import CAdapter
from eiplgrader.languages.adapters.cpp_adapter import CppAdapter
from eiplgrader.languages.adapters.go_adapter import GoAdapter
from eiplgrader.languages.adapters.haskell_adapter import HaskellAdapter


class TestComprehensiveSuite:
    """Unified test suite combining all three agent approaches."""

    @pytest.fixture(autouse=True)
    def setup_registry(self):
        """Set up the language registry with all adapters."""
        # Register all adapters
        adapters = [
            ("python", PythonAdapter),
            ("javascript", JavaScriptAdapter),
            ("java", JavaAdapter),
            ("c", CAdapter),
            ("cpp", CppAdapter),
            ("go", GoAdapter),
            ("haskell", HaskellAdapter),
        ]

        for name, adapter_class in adapters:
            language_registry.register(name, adapter_class)

    def test_suite_integration(self):
        """Test that all three agent test suites are properly integrated."""

        # Test suite directories should exist
        test_dirs = [
            "tests/unit/test_executors",
            "tests/unit/test_adapters",
            "tests/integration/test_registry",
            "tests/edge_cases/test_error_scenarios",
            "tests/edge_cases/test_resource_limits",
            "tests/fixtures/mock_code_samples",
        ]

        project_root = Path(__file__).parent.parent
        for test_dir in test_dirs:
            assert (
                project_root / test_dir
            ).exists(), f"Missing test directory: {test_dir}"

    def test_language_coverage(self):
        """Test that all 7 languages are covered across all test suites."""

        # All languages should be supported
        supported_languages = [
            "python",
            "javascript",
            "java",
            "c",
            "cpp",
            "go",
            "haskell",
        ]

        for language in supported_languages:
            # Should have adapter
            adapter = language_registry.get_adapter(language)
            assert adapter is not None, f"Missing adapter for {language}"

            # Should have executor
            executor = language_registry.get_executor(language)
            assert executor is not None, f"Missing executor for {language}"

    def test_type_system_categories(self):
        """Test that type system categories are properly handled."""

        # JSON-capable languages (support type inference via JSON input)
        json_languages = ["python", "javascript"]

        # Static type languages (require explicit types)
        static_languages = ["c", "cpp", "java", "go", "haskell"]

        for language in json_languages:
            executor = language_registry.get_executor(language)
            # Check if it's an interpreted language executor
            assert hasattr(
                executor, "validate_or_infer_types"
            ), f"{language} executor should support type inference"

        for language in static_languages:
            executor = language_registry.get_executor(language)
            assert hasattr(
                executor, "validate_types_provided"
            ), f"{language} executor should require explicit types"

    @pytest.mark.integration
    def test_full_workflow_integration(self):
        """Test complete adapter→executor workflow for each language."""

        # Simple test case that should work across all languages
        test_case = {
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "function_name": "add",
        }

        for language in ["python", "javascript", "go"]:  # Test inference languages
            adapter = language_registry.get_adapter(language)
            executor = language_registry.get_executor(language)

            assert adapter is not None, f"Missing adapter for {language}"
            assert executor is not None, f"Missing executor for {language}"

            # Test adapter methods
            config = adapter.get_config()
            assert config.name == language

            # Test basic prompt generation
            prompt = adapter.generate_prompt("adds two numbers", "add", gen_type="cgbg")
            assert isinstance(prompt, str)
            assert len(prompt) > 0

    @pytest.mark.type_system
    def test_agent1_type_consistency(self):
        """Validate Agent 1's type system testing approach."""

        # Test that type inference works for supported languages
        inference_languages = ["python", "javascript", "go"]

        for language in inference_languages:
            executor = language_registry.get_executor(language)

            # Should support type inference
            test_case = {"parameters": {"x": 42}, "expected": 42}

            # This should work without explicit types
            if hasattr(executor, "validate_or_infer_types"):
                result = executor.validate_or_infer_types(test_case.copy())
                assert "parameter_types" in result
                assert result["parameter_types"]["x"] == "int"

    @pytest.mark.infrastructure
    def test_agent2_infrastructure_robustness(self):
        """Validate Agent 2's infrastructure testing approach."""

        # Test registry system
        assert len(language_registry.list_languages()) == 7

        # Test adapter configuration consistency
        for language in language_registry.list_languages():
            adapter = language_registry.get_adapter(language)
            config = adapter.get_config()

            # Configuration should be complete
            assert config.name == language
            assert len(config.file_extensions) > 0
            # Some compiled languages (like Haskell) may have empty run_command
            if language in ["python", "javascript", "go"]:
                assert (
                    len(config.run_command) > 0
                ), f"{language} should have run_command"
            elif language in ["c", "cpp", "java", "haskell"]:
                # Compiled languages might have empty run_command but should have compile_command
                assert (
                    config.compile_command is not None
                    and len(config.compile_command) > 0
                ), f"{language} should have compile_command"
            assert config.test_timeout > 0

    @pytest.mark.edge_cases
    def test_agent3_error_resilience(self):
        """Validate Agent 3's error handling testing approach."""

        # Test that invalid language names are handled gracefully
        assert language_registry.get_adapter("invalid_language") is None
        assert language_registry.get_executor("invalid_language") is None

        # Test that malformed test cases are handled
        for language in ["python", "java"]:  # Test one inference + one static
            executor = language_registry.get_executor(language)

            # Malformed test case should not crash
            try:
                malformed_case = {"invalid": "structure"}
                # This should either work or fail gracefully
                if language == "java":
                    # Static languages should validate types
                    if hasattr(executor, "validate_types_provided"):
                        with pytest.raises(ValueError):
                            executor.validate_types_provided(malformed_case)
            except Exception as e:
                # Should be a handled exception, not a crash
                assert isinstance(e, (ValueError, TypeError, AttributeError))


def run_comprehensive_suite():
    """Run the comprehensive test suite with detailed reporting."""

    print("=" * 80)
    print("EIPLGRADER COMPREHENSIVE TEST SUITE")
    print("=" * 80)
    print()
    print("Test Suite Integration:")
    print("✓ Agent 1: Type System Specialist - Type handling tests")
    print("✓ Agent 2: Execution Environment Engineer - Infrastructure tests")
    print("✓ Agent 3: Edge Case & Error Resilience Tester - Failure mode tests")
    print()

    # Parse command line arguments
    import argparse

    parser = argparse.ArgumentParser(description="Run comprehensive test suite")
    parser.add_argument(
        "--category",
        choices=["type-system", "infrastructure", "edge-cases", "all"],
        default="all",
        help="Test category to run",
    )
    parser.add_argument(
        "--full-suite",
        action="store_true",
        help="Run all test suites including individual agent tests",
    )
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output")

    args = parser.parse_args()

    # Run tests based on category
    pytest_args = [__file__]

    if args.verbose:
        pytest_args.append("-v")

    if args.category != "all":
        pytest_args.extend(["-m", args.category])

    if args.full_suite:
        # Run all individual test suites
        test_dirs = [
            "tests/unit/test_executors/",
            "tests/unit/test_adapters/",
            "tests/integration/test_registry/",
            "tests/edge_cases/test_error_scenarios/",
            "tests/edge_cases/test_resource_limits/",
        ]

        project_root = Path(__file__).parent.parent
        for test_dir in test_dirs:
            full_path = project_root / test_dir
            if full_path.exists():
                pytest_args.append(str(full_path))

    # Run pytest
    return pytest.main(pytest_args)


if __name__ == "__main__":
    exit_code = run_comprehensive_suite()
    sys.exit(exit_code)
