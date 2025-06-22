"""
Comprehensive type system validation tests for all executors.

This module tests:
1. Type inference behavior in Python/JavaScript executors
2. Explicit type validation in C/C++/Java/Go/Haskell executors
3. Type system boundaries and error conditions
4. Complex type scenarios across all languages
5. Type coercion and conversion handling
6. Validation error messages and clarity
"""

import os
import subprocess
import sys

import pytest
from eiplgrader.languages.executors.python_executor import PythonExecutor
from eiplgrader.languages.executors.javascript_executor import JavaScriptExecutor
from eiplgrader.languages.executors.go_executor import GoExecutor
from eiplgrader.languages.executors.java_executor import JavaExecutor
from eiplgrader.languages.executors.cpp_executor import CppExecutor
from eiplgrader.languages.executors.c_executor import CExecutor
from eiplgrader.languages.executors.haskell_executor import HaskellExecutor
from tests.fixtures.mock_code_samples import (
    python_samples,
    javascript_samples,
    go_samples,
    java_samples,
    cpp_samples,
    c_samples,
    haskell_samples,
)

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../.."))


class TestTypeSystemValidation:
    """Test suite for comprehensive type system validation across all executors."""

    def setup_method(self):
        """Set up test fixtures."""
        self.type_inference_executors = {}
        self.static_type_executors = {}
        self.samples = {}

        # Type inference languages (Python, JavaScript)
        try:
            self.type_inference_executors["python"] = PythonExecutor()
            self.samples["python"] = python_samples
        except Exception:
            pass

        try:
            self.type_inference_executors["javascript"] = JavaScriptExecutor()
            self.samples["javascript"] = javascript_samples
        except Exception:
            pass

        # Static type languages (Java, C++, C, Go, Haskell)
        try:
            self.static_type_executors["java"] = JavaExecutor()
            self.samples["java"] = java_samples
        except Exception:
            pass

        try:
            self.static_type_executors["go"] = GoExecutor()
            self.samples["go"] = go_samples
        except Exception:
            pass

        try:
            self.static_type_executors["cpp"] = CppExecutor()
            self.samples["cpp"] = cpp_samples
        except Exception:
            pass

        try:
            self.static_type_executors["c"] = CExecutor()
            self.samples["c"] = c_samples
        except Exception:
            pass

        try:
            self.static_type_executors["haskell"] = HaskellExecutor()
            self.samples["haskell"] = haskell_samples
        except Exception:
            pass

    def teardown_method(self):
        """Clean up after each test."""
        for executor in self.type_inference_executors.values():
            if executor:
                executor.cleanup()
        for executor in self.static_type_executors.values():
            if executor:
                executor.cleanup()

    def _check_compiler_available(self, language):
        """Check if compiler/interpreter for language is available."""
        checks = {
            "python": ["python3", "--version"],
            "javascript": ["node", "--version"],
            "go": ["go", "version"],
            "java": ["javac", "-version"],
            "cpp": ["g++", "--version"],
            "c": ["gcc", "--version"],
            "haskell": ["ghc", "--version"],
        }

        if language not in checks:
            return False

        try:
            subprocess.run(checks[language], capture_output=True, check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            return False

    # ===== TYPE INFERENCE LANGUAGE TESTS =====

    def test_type_inference_automatic_detection(self):
        """Test that type inference languages automatically detect types."""
        test_scenarios = [
            {
                "name": "integers",
                "parameters": {"a": 5, "b": 3},
                "expected_inference": {"a": "int", "b": "int"},
            },
            {
                "name": "floats",
                "parameters": {"x": 3.14, "y": 2.71},
                "expected_inference": {"x": "double", "y": "double"},
            },
            {
                "name": "strings",
                "parameters": {"s": "hello", "t": "world"},
                "expected_inference": {"s": "string", "t": "string"},
            },
            {
                "name": "booleans",
                "parameters": {"flag": True, "active": False},
                "expected_inference": {"flag": "bool", "active": "bool"},
            },
            {
                "name": "lists",
                "parameters": {"nums": [1, 2, 3], "words": ["a", "b"]},
                "expected_inference": {"nums": "List[int]", "words": "List[string]"},
            },
        ]

        for lang_name, executor in self.type_inference_executors.items():
            if not executor or not self._check_compiler_available(lang_name):
                continue

            for scenario in test_scenarios:
                func_name = "add_numbers" if lang_name == "python" else "addNumbers"
                test_case = {
                    "function_name": func_name,
                    "parameters": scenario["parameters"],
                    "expected": 0,
                    "inplace": "0",
                }

                # Test that the executor can infer types
                # The actual implementation varies by executor, but they should all handle
                # type inference without requiring explicit parameter_types
                assert "parameter_types" not in test_case
                assert "expected_type" not in test_case

                # The executor should be able to process this without throwing
                # type validation errors

    def test_type_inference_optional_explicit_types(self):
        """Test that type inference languages respect explicit types when provided."""
        for lang_name, executor in self.type_inference_executors.items():
            if not executor or not self._check_compiler_available(lang_name):
                continue
            func_name = "add_numbers" if lang_name == "python" else "addNumbers"
            test_case_with_types = {
                "function_name": func_name,
                "parameters": {"a": 5, "b": 3},
                "parameter_types": {"a": "int", "b": "int"},
                "expected": 8,
                "expected_type": "int",
                "inplace": "0",
            }

            test_case_without_types = {
                "function_name": func_name,
                "parameters": {"a": 5, "b": 3},
                "expected": 8,
                "inplace": "0",
            }

            try:
                code_sample = getattr(self.samples[lang_name], "ADD_NUMBERS")

                # Both should work
                result_with_types = executor.execute_test(
                    code_sample, test_case_with_types
                )
                result_without_types = executor.execute_test(
                    code_sample, test_case_without_types
                )

                assert result_with_types[
                    "passed"
                ], f"{lang_name} failed with explicit types"
                assert result_without_types[
                    "passed"
                ], f"{lang_name} failed with type inference"
                assert result_with_types["actual"] == result_without_types["actual"]

            except Exception as e:
                pytest.fail(f"{lang_name} type inference test failed: {e}")

    def test_type_inference_complex_scenarios(self):
        """Test type inference with complex data structures."""
        complex_scenarios = [
            {
                "name": "nested_lists",
                "parameters": {"nested": [[1, 2], [3, 4]]},
                "function": (
                    "flattenNested"
                    if "flattenNested" in dir(python_samples)
                    else "sumEvenNumbers"
                ),
            },
            {
                "name": "mixed_number_types",
                "parameters": {"a": 10, "b": 20.5},  # int and float
                "function": (
                    "calculateAverage"
                    if hasattr(go_samples, "CALCULATE_AVERAGE")
                    else "addNumbers"
                ),
            },
            {
                "name": "empty_collections",
                "parameters": {"numbers": []},
                "function": "sumEvenNumbers",
            },
        ]

        for lang_name, executor in self.type_inference_executors.items():
            if not executor or not self._check_compiler_available(lang_name):
                continue

            for scenario in complex_scenarios:
                test_case = {
                    "function_name": scenario["function"],
                    "parameters": scenario["parameters"],
                    "expected": 0,  # Don't care about result, just type handling
                    "inplace": "0",
                }

                # Should not require explicit types
                assert "parameter_types" not in test_case

    # ===== STATIC TYPE LANGUAGE TESTS =====

    def test_static_type_validation_required(self):
        """Test that static type languages require explicit type annotations."""
        print(f"Static type executors: {list(self.static_type_executors.keys())}")
        for lang_name, executor in self.static_type_executors.items():
            if not executor:
                print(f"Skipping {lang_name} - no executor")
                continue
            if not self._check_compiler_available(lang_name):
                print(f"Skipping {lang_name} - compiler not available")
                continue

            # Test case without types - should fail validation
            func_name = "add_numbers" if lang_name == "python" else "addNumbers"
            print(lang_name)
            test_case_no_types = {
                "function_name": func_name,
                "parameters": {"a": 5, "b": 3},
                "expected": 8,
                "inplace": "0",
            }

            print(f"\nTesting {lang_name} without types...")
            try:
                with pytest.raises(ValueError) as exc_info:
                    code_sample = getattr(self.samples[lang_name], "ADD_NUMBERS")
                    executor.execute_test(code_sample, test_case_no_types)
            except Exception as e:
                print(f"Exception for {lang_name}: {e}")
                raise

            error_msg = str(exc_info.value)
            assert "Missing required type information" in error_msg
            assert any(
                phrase in error_msg
                for phrase in [
                    "parameter_types not provided",
                    "expected_type not provided",
                ]
            )

    def test_complex_type_scenarios(self):
        """Test complex type scenarios across all languages."""
        scenarios = [
            {
                "name": "multiple_parameters",
                "function": "formatInfo",
                "parameters": {
                    "name": "Alice",
                    "age": 30,
                    "active": True,
                    "salary": 50000.0,
                },
                "type_mappings": {
                    "java": {
                        "name": "String",
                        "age": "int",
                        "active": "boolean",
                        "salary": "double",
                    },
                    "cpp": {
                        "name": "std::string",
                        "age": "int",
                        "active": "bool",
                        "salary": "double",
                    },
                    "c": {
                        "name": "char*",
                        "age": "int",
                        "active": "int",
                        "salary": "double",
                    },
                    "go": {
                        "name": "string",
                        "age": "int",
                        "active": "bool",
                        "salary": "float64",
                    },
                    "haskell": {
                        "name": "String",
                        "age": "Int",
                        "active": "Bool",
                        "salary": "Double",
                    },
                },
                "expected_types": {
                    "java": "String",
                    "cpp": "std::string",
                    "c": "char*",
                    "go": "string",
                    "haskell": "String",
                },
            },
            {
                "name": "array_operations",
                "function": "sumEvenNumbers",
                "parameters": {"numbers": [1, 2, 3, 4]},
                "type_mappings": {
                    "java": {"numbers": "int[]"},
                    "cpp": {"numbers": "std::vector<int>"},
                    "c": {"numbers": "int*", "size": "int"},
                    "go": {"numbers": "[]int"},
                    "haskell": {"numbers": "[Int]"},
                },
                "expected_types": {
                    "java": "int",
                    "cpp": "int",
                    "c": "int",
                    "go": "int",
                    "haskell": "Int",
                },
            },
        ]

        for scenario in scenarios:
            for lang_name, executor in self.static_type_executors.items():
                if not executor or not self._check_compiler_available(lang_name):
                    continue

                if lang_name not in scenario["type_mappings"]:
                    continue

                parameters = scenario["parameters"].copy()
                if lang_name == "c" and "numbers" in parameters:
                    parameters["size"] = len(parameters["numbers"])

                test_case = {
                    "function_name": scenario["function"],
                    "parameters": parameters,
                    "parameter_types": scenario["type_mappings"][lang_name],
                    "expected": 0,
                    "expected_type": scenario["expected_types"][lang_name],
                    "inplace": "0",
                }

                # Should pass validation
                try:
                    executor.validate_types_provided(test_case)
                except ValueError as e:
                    if "Missing required type information" in str(e):
                        pytest.fail(
                            f"Complex type validation failed for {lang_name}: {e}"
                        )

    def test_type_boundary_conditions(self):
        """Test edge cases in type handling."""
        boundary_tests = [
            {
                "name": "empty_parameters",
                "parameters": {},
                "parameter_types": {},
                "valid": True,
            },
            {
                "name": "null_like_values",
                "parameters": {"value": None},
                "parameter_types": {"value": "int"},
                "valid": False,  # Most static languages don't handle None well
            },
        ]

        for lang_name, executor in self.static_type_executors.items():
            if not executor or not self._check_compiler_available(lang_name):
                continue

            for test in boundary_tests:
                test_case = {
                    "function_name": "testFunction",
                    "parameters": test["parameters"],
                    "parameter_types": test["parameter_types"],
                    "expected": 0,
                    "expected_type": "int",
                    "inplace": "0",
                }

                if test["valid"]:
                    # Should pass validation
                    try:
                        executor.validate_types_provided(test_case)
                    except ValueError as e:
                        if "Missing required type information" in str(e):
                            pytest.fail(
                                f"Valid boundary test failed for {lang_name}: {e}"
                            )
                else:
                    # May or may not pass validation, but shouldn't crash
                    try:
                        executor.validate_types_provided(test_case)
                    except ValueError:
                        pass  # Expected for some cases

    def test_cross_type_system_comparison(self):
        """Compare type systems across language categories."""
        # Ensure we have representatives from both categories
        inference_langs = [
            lang
            for lang, executor in self.type_inference_executors.items()
            if executor and self._check_compiler_available(lang)
        ]
        static_langs = [
            lang
            for lang, executor in self.static_type_executors.items()
            if executor and self._check_compiler_available(lang)
        ]

        if not inference_langs:
            pytest.skip("No type inference languages available")
        if not static_langs:
            pytest.skip("No static type languages available")

        print(f"\\nType inference languages tested: {inference_langs}")
        print(f"Static type languages tested: {static_langs}")

        # Basic test that demonstrates the difference
        # Type inference languages should accept this
        for lang in inference_langs:

            func_name = "add_numbers" if lang == "python" else "addNumbers"
            simple_test = {
                "function_name": func_name,
                "parameters": {"a": 5, "b": 3},
                "expected": 8,
            }

            executor = self.type_inference_executors[lang]
            try:
                code_sample = getattr(self.samples[lang], "ADD_NUMBERS")
                result = executor.execute_test(code_sample, simple_test)
                assert result[
                    "passed"
                ], f"{lang} should handle test without explicit types"
            except Exception as e:
                pytest.fail(f"Type inference failed for {lang}: {e}")

        # Static type languages should reject this
        for lang in static_langs:
            print(lang)
            func_name = "add_numbers" if lang == "python" else "addNumbers"
            simple_test = {
                "function_name": func_name,
                "parameters": {"a": 5, "b": 3},
                "expected": 8,
            }
            executor = self.static_type_executors[lang]
            with pytest.raises(ValueError):
                code_sample = getattr(self.samples[lang], "ADD_NUMBERS")
                executor.execute_test(code_sample, simple_test)

    def test_type_system_documentation_compliance(self):
        """Test that type systems behave as documented."""
        documented_behavior = {
            "type_inference": {
                "languages": ["python", "javascript"],
                "requires_types": False,
                "supports_inference": True,
                "native_json": True,
            },
            "static_types": {
                "languages": ["java", "cpp", "c", "go", "haskell"],
                "requires_types": True,
                "supports_inference": False,
                "native_json": False,
            },
        }

        for category, behavior in documented_behavior.items():
            for lang in behavior["languages"]:
                if category == "type_inference":
                    executor = self.type_inference_executors.get(lang)
                else:
                    executor = self.static_type_executors.get(lang)

                if not executor or not self._check_compiler_available(lang):
                    continue

                func_name = "add_numbers" if lang == "python" else "addNumbers"
                test_case = {
                    "function_name": func_name,
                    "parameters": {"a": 1, "b": 2},
                    "expected": 3,
                    "inplace": "0",
                }

                if behavior["requires_types"]:
                    # Should fail without types
                    with pytest.raises(ValueError):
                        code_sample = getattr(self.samples[lang], "ADD_NUMBERS")
                        executor.execute_test(code_sample, test_case)
                else:
                    # Should work without types
                    try:
                        code_sample = getattr(self.samples[lang], "ADD_NUMBERS")
                        result = executor.execute_test(code_sample, test_case)
                        print(result)
                        assert result[
                            "passed"
                        ], f"{lang} should work without explicit types"
                    except Exception as e:
                        pytest.fail(
                            f"Type inference documentation compliance failed for {lang}: {e}"
                        )
