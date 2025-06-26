#!/usr/bin/env python3
"""
EiplGrader Documentation Example Validator
==========================================

This script validates all code examples found in the documentation to ensure they
work correctly with the actual codebase implementation.

Setup: Set your OPENAI_API_KEY environment variable or create a .env file with:
OPENAI_API_KEY=your_api_key_here

Features:
- Tests all API examples from documentation
- Validates CodeGenerator, CodeTester, and LanguageRegistry
- Checks error handling examples
- Tests various test case formats
- Comprehensive validation of return types and data structures
- Safe to run (no API calls without explicit key)
"""

import os
import sys
import tempfile
import traceback
from typing import Dict, List, Any, Optional
import json

# Add the project root to Python path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester
from eiplgrader.languages.registry import LanguageRegistry
import dotenv

# Load environment variables
dotenv.load_dotenv()


class DocumentationExampleValidator:
    """Validates all examples in the documentation."""
    
    def __init__(self):
        self.results = []
        self.api_key = os.getenv("OPENAI_API_KEY")
        if not self.api_key:
            print("Warning: OPENAI_API_KEY not set. Code generation examples will be skipped.")
            print("  To test code generation:")
            print("  1. Create a .env file in this directory with: OPENAI_API_KEY=your_api_key_here")
            print("  2. Or set the environment variable: export OPENAI_API_KEY=your_api_key_here")
    
    def run_all_examples(self):
        """Run all documentation examples."""
        print("🚀 Starting documentation example validation...")
        print("=" * 60)
        
        # Basic usage examples
        self.test_basic_usage_examples()
        
        # CodeGenerator examples
        self.test_codegen_examples()
        
        # CodeTester examples
        self.test_tester_examples()
        
        # Language Registry examples
        self.test_language_registry_examples()
        
        # Test case format examples
        self.test_test_case_examples()
        
        # Error handling examples
        self.test_error_handling_examples()
        
        # Advanced examples
        self.test_advanced_examples()
        
        # Print summary
        return self.print_summary()
    
    def test_basic_usage_examples(self):
        """Test basic usage examples from docs/index.md"""
        print("\n📚 Testing Basic Usage Examples")
        print("-" * 40)
        
        # Example from docs/index.md
        test_name = "Basic CodeTester Example"
        try:
            # Create a simple test without code generation
            test_cases = [
                {"parameters": {"a": 1, "b": 2}, "expected": 3},
                {"parameters": {"a": -1, "b": 1}, "expected": 0}
            ]
            
            # Simple Python function for testing
            simple_code = "def add_numbers(a, b):\n    return a + b"
            
            code_tester = CodeTester(
                code=simple_code,
                test_cases=test_cases,
                function_name="add_numbers",
                language="python"
            )
            
            test_result = code_tester.run_tests()
            
            # Verify the result structure matches documentation
            assert hasattr(test_result, 'successes'), "CodeTestResult should have 'successes' attribute"
            assert hasattr(test_result, 'testsRun'), "CodeTestResult should have 'testsRun' property"
            assert hasattr(test_result, 'was_successful'), "CodeTestResult should have 'was_successful' method"
            assert test_result.successes == 2, f"Expected 2 successes, got {test_result.successes}"
            assert test_result.testsRun == 2, f"Expected 2 tests run, got {test_result.testsRun}"
            assert test_result.was_successful(), "All tests should pass"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
    
    def test_codegen_examples(self):
        """Test CodeGenerator examples from API documentation"""
        print("\n🔧 Testing CodeGenerator Examples")
        print("-" * 40)
        
        # Test structure validation (without API key)
        test_name = "CodeGenerator Structure Validation"
        try:
            # Just verify the method exists with correct signature
            generator = CodeGenerator(
                api_key="dummy-key",  # Won't actually call API
                client_type="openai",
                language="python"
            )
            
            # Verify method exists
            assert hasattr(generator, 'generate_code'), "CodeGenerator should have generate_code method"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
        
        if not self.api_key:
            print("⚠️  Skipping API-dependent CodeGenerator examples (no API key)")
            return
        
        # Example from docs/developer/api/codegen-api.md
        test_name = "CodeGenerator Basic Usage"
        try:
            generator = CodeGenerator(
                api_key=self.api_key,
                client_type="openai",
                language="python"
            )
            
            # Verify the constructor worked
            assert generator.api_key == self.api_key
            assert generator.client_type == "openai"
            assert generator.language == "python"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
        
        # Test actual code generation (simple example to minimize API costs)
        test_name = "CodeGenerator Full Integration"
        try:
            generator = CodeGenerator(
                api_key=self.api_key,
                client_type="openai",
                language="python"
            )
            
            # Generate a simple function
            result = generator.generate_code(
                student_response="adds two numbers together",
                function_name="add_numbers",
                gen_type="cgbg",
                num_to_gen=1
            )
            
            # Verify result structure
            assert isinstance(result, dict), "generate_code should return a dict"
            assert "code" in result, "Result should contain 'code' key"
            assert isinstance(result["code"], list), "Code should be a list"
            assert len(result["code"]) == 1, "Should generate exactly 1 implementation"
            
            # Test the generated code
            generated_code = result["code"][0]
            test_cases = [
                {"parameters": {"a": 1, "b": 2}, "expected": 3},
                {"parameters": {"a": -1, "b": 1}, "expected": 0}
            ]
            
            tester = CodeTester(
                code=generated_code,
                test_cases=test_cases,
                function_name="add_numbers",
                language="python"
            )
            
            test_result = tester.run_tests()
            
            # The generated code should work (though not guaranteed to be perfect)
            print(f"   Generated code test result: {test_result.successes}/{test_result.testsRun} passed")
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
    
    def test_tester_examples(self):
        """Test CodeTester examples from API documentation"""
        print("\n🧪 Testing CodeTester Examples")
        print("-" * 40)
        
        # Example from docs/developer/api/tester-api.md
        test_name = "CodeTester Constructor"
        try:
            code = "def factorial(n):\n    if n <= 1:\n        return 1\n    return n * factorial(n-1)"
            test_cases = [
                {"parameters": {"n": 5}, "expected": 120},
                {"parameters": {"n": 0}, "expected": 1}
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="factorial",
                language="python"
            )
            
            # Verify constructor parameters
            assert tester.code == code
            assert tester.test_cases == test_cases
            assert tester.function_name == "factorial"
            assert tester.language == "python"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
        
        # Test run_tests method
        test_name = "CodeTester run_tests"
        try:
            code = "def factorial(n):\n    if n <= 1:\n        return 1\n    return n * factorial(n-1)"
            test_cases = [
                {"parameters": {"n": 5}, "expected": 120},
                {"parameters": {"n": 0}, "expected": 1}
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="factorial",
                language="python"
            )
            
            results = tester.run_tests()
            
            # Verify return type matches documentation
            from eiplgrader.tester import CodeTestResult
            assert isinstance(results, CodeTestResult), f"Expected CodeTestResult, got {type(results)}"
            
            # Verify documented methods exist
            assert hasattr(results, 'was_successful'), "CodeTestResult should have was_successful method"
            assert hasattr(results, 'test_results'), "CodeTestResult should have test_results attribute"
            assert hasattr(results, 'successes'), "CodeTestResult should have successes attribute"
            assert hasattr(results, 'testsRun'), "CodeTestResult should have testsRun property"
            
            # Verify results structure
            assert results.was_successful(), "Factorial tests should pass"
            assert results.successes == 2, f"Expected 2 successes, got {results.successes}"
            assert results.testsRun == 2, f"Expected 2 tests, got {results.testsRun}"
            
            # Verify test_results structure matches documentation
            assert isinstance(results.test_results, list), "test_results should be a list"
            for result in results.test_results:
                assert isinstance(result, dict), "Each test result should be a dictionary"
                # Check actual keys used in implementation
                required_keys = ["function_call", "expected_output", "actual_output", "pass", "error"]
                for key in required_keys:
                    assert key in result, f"Test result missing required key: {key}"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
    
    def test_language_registry_examples(self):
        """Test LanguageRegistry examples from API documentation"""
        print("\n🌐 Testing Language Registry Examples")
        print("-" * 40)
        
        # Example from docs/developer/api/language-api.md
        test_name = "LanguageRegistry Basic Usage"
        try:
            # Use the global registry which has languages already registered
            from eiplgrader.languages import language_registry
            registry = language_registry
            
            # Test list_languages method
            languages = registry.list_languages()
            assert isinstance(languages, list), "list_languages should return a list"
            assert "python" in languages, "Python should be in supported languages"
            
            # Test is_supported method
            assert registry.is_supported("python"), "Python should be supported"
            
            # Test get_adapter method
            adapter = registry.get_adapter("python")
            assert adapter is not None, "Should get Python adapter"
            
            # Test get_executor method
            executor_class = registry.get_executor("python")
            assert executor_class is not None, "Should get Python executor class"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
    
    def test_test_case_examples(self):
        """Test various test case formats from documentation"""
        print("\n📋 Testing Test Case Examples")
        print("-" * 40)
        
        # Python test case (dynamic language)
        test_name = "Python Test Case Format"
        try:
            test_cases = [
                {
                    "parameters": {"x": 5, "y": "hello"},
                    "expected": "hello5",
                    "function_name": "concat"
                }
            ]
            
            code = "def concat(x, y):\n    return str(y) + str(x)"
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="concat",
                language="python"
            )
            
            results = tester.run_tests()
            assert results.was_successful(), "Python test case should pass"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
        
        # Test case with inplace parameter
        test_name = "Inplace Test Case Format"
        try:
            test_cases = [
                {
                    "parameters": {"arr": [1, 2, 3]},
                    "expected": [1, 2, 3, 4],
                    "inplace": "0"  # Normal return mode
                }
            ]
            
            code = "def append_four(arr):\n    return arr + [4]"
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="append_four",
                language="python"
            )
            
            results = tester.run_tests()
            assert results.was_successful(), "Inplace test case should pass"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
    
    def test_error_handling_examples(self):
        """Test error handling examples from documentation"""
        print("\n⚠️  Testing Error Handling Examples")
        print("-" * 40)
        
        # Test with failing code
        test_name = "Error Handling - Failing Tests"
        try:
            code = "def add(a, b):\n    return a + b + 1"  # Wrong implementation
            test_cases = [
                {"parameters": {"a": 1, "b": 2}, "expected": 3}
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="add",
                language="python"
            )
            
            results = tester.run_tests()
            
            # Should fail as expected
            assert not results.was_successful(), "Test should fail with wrong implementation"
            assert results.failures > 0, "Should have failures"
            
            # Verify error structure
            failed_tests = [r for r in results.test_results if not r["pass"]]
            assert len(failed_tests) == 1, "Should have exactly one failed test"
            
            failed_test = failed_tests[0]
            assert failed_test["expected_output"] == 3, "Expected value should be 3"
            assert failed_test["actual_output"] == 4, "Actual value should be 4"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
        
        # Test with syntax error
        test_name = "Error Handling - Syntax Error"
        try:
            code = "def broken_func(\n    return 42"  # Syntax error
            test_cases = [
                {"parameters": {}, "expected": 42}
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="broken_func",
                language="python"
            )
            
            results = tester.run_tests()
            
            # Should have errors (syntax errors are treated as errors, not failures)
            assert not results.was_successful(), "Test should fail with syntax error"
            # Check that we have either errors or failures (implementation may vary)
            assert (results.errors > 0 or results.failures > 0), "Should have errors or failures"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
    
    def test_advanced_examples(self):
        """Test advanced examples from documentation"""
        print("\n🚀 Testing Advanced Examples")
        print("-" * 40)
        
        # Test multiple test cases
        test_name = "Multiple Test Cases"
        try:
            code = """
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
"""
            
            test_cases = [
                {"parameters": {"n": 0}, "expected": 0},
                {"parameters": {"n": 1}, "expected": 1},
                {"parameters": {"n": 5}, "expected": 5},
                {"parameters": {"n": 10}, "expected": 55}
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="fibonacci",
                language="python"
            )
            
            results = tester.run_tests()
            
            assert results.was_successful(), "Fibonacci tests should all pass"
            assert results.testsRun == 4, "Should run 4 tests"
            assert results.successes == 4, "All 4 tests should succeed"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
        
        # Test complex data structures
        test_name = "Complex Data Structures"
        try:
            code = """
def process_data(data):
    result = {}
    for key, values in data.items():
        result[key] = sum(values) / len(values)
    return result
"""
            
            test_cases = [
                {
                    "parameters": {
                        "data": {
                            "group1": [1, 2, 3, 4, 5],
                            "group2": [10, 20, 30]
                        }
                    },
                    "expected": {"group1": 3.0, "group2": 20.0}
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="process_data",
                language="python"
            )
            
            results = tester.run_tests()
            
            assert results.was_successful(), "Complex data structure test should pass"
            
            self.record_success(test_name)
            
        except Exception as e:
            self.record_failure(test_name, e)
    
    def record_success(self, test_name: str):
        """Record a successful test."""
        self.results.append({"name": test_name, "status": "PASS", "error": None})
        print(f"✅ {test_name}")
    
    def record_failure(self, test_name: str, error: Exception):
        """Record a failed test."""
        self.results.append({"name": test_name, "status": "FAIL", "error": str(error)})
        print(f"❌ {test_name}: {error}")
    
    def print_summary(self):
        """Print test summary."""
        print("\n" + "=" * 60)
        print("📊 VALIDATION SUMMARY")
        print("=" * 60)
        
        passed = sum(1 for r in self.results if r["status"] == "PASS")
        failed = sum(1 for r in self.results if r["status"] == "FAIL")
        total = len(self.results)
        
        print(f"Total Tests: {total}")
        print(f"✅ Passed: {passed}")
        print(f"❌ Failed: {failed}")
        print(f"Success Rate: {(passed/total)*100:.1f}%")
        
        if failed > 0:
            print("\n🔍 FAILED TESTS:")
            print("-" * 40)
            for result in self.results:
                if result["status"] == "FAIL":
                    print(f"❌ {result['name']}")
                    print(f"   Error: {result['error']}")
                    print()
        
        print("\n" + "=" * 60)
        
        if failed == 0:
            print("🎉 All documentation examples are working correctly!")
        else:
            print(f"⚠️  {failed} examples need attention.")
            return 1
        
        return 0


def main():
    """Main entry point."""
    print("EiplGrader Documentation Example Validator")
    print("==========================================")
    
    validator = DocumentationExampleValidator()
    exit_code = validator.run_all_examples()
    
    return exit_code


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)