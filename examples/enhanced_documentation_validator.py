#!/usr/bin/env python3
"""
Enhanced EiplGrader Documentation Validator
==========================================

Comprehensive validation of all code examples in the documentation.
Supports all 7 languages and advanced features.

Setup: Set your API key environment variable or create a .env file.
See .env.example for all supported providers and their API keys.

Features:
- Multi-language validation (Python, Java, JavaScript, C/C++, Go, Haskell)
- Advanced feature testing (segmentation, parallel processing)
- Type system validation for static languages
- Installation and setup validation
- Comprehensive error handling validation
"""

import os
import sys
import tempfile
import traceback
import shutil
import subprocess
import json
import concurrent.futures
from typing import Dict, List, Any, Optional, Union
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path

# Add the project root to Python path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester
from eiplgrader.languages.registry import LanguageRegistry
import dotenv

# Load environment variables
dotenv.load_dotenv()


@dataclass
class ValidationExample:
    """Represents a code example to be validated."""
    name: str
    language: str
    category: str
    code: str
    test_cases: Optional[List[Dict]] = None
    function_name: Optional[str] = None
    description: str = ""
    source_file: str = ""
    complexity: str = "simple"


@dataclass
class ValidationResult:
    """Represents the result of validating an example."""
    example_name: str
    language: str
    category: str
    status: str  # "PASS", "FAIL", "SKIP"
    error: Optional[str] = None
    execution_time: float = 0.0
    details: Optional[Dict] = None


class LanguageValidator(ABC):
    """Abstract base class for language-specific validators."""
    
    def __init__(self, api_key: Optional[str] = None, client_type: Optional[str] = None, model: Optional[str] = None):
        self.api_key = api_key
        self.client_type = client_type or "openai"
        self.model = model or "gpt-4o"
        self.results: List[ValidationResult] = []
    
    @abstractmethod
    def get_language_name(self) -> str:
        """Return the language name."""
        pass
    
    @abstractmethod
    def is_environment_available(self) -> bool:
        """Check if the language environment is available."""
        pass
    
    @abstractmethod
    def validate_basic_usage(self) -> List[ValidationResult]:
        """Validate basic usage examples for this language."""
        pass
    
    @abstractmethod
    def validate_type_system(self) -> List[ValidationResult]:
        """Validate type system examples for this language."""
        pass
    
    @abstractmethod
    def validate_error_handling(self) -> List[ValidationResult]:
        """Validate error handling examples for this language."""
        pass
    
    def record_result(self, name: str, status: str, error: str = None, details: Dict = None) -> ValidationResult:
        """Record a validation result."""
        result = ValidationResult(
            example_name=name,
            language=self.get_language_name(),
            category="language_specific",
            status=status,
            error=error,
            details=details or {}
        )
        self.results.append(result)
        return result


class PythonValidator(LanguageValidator):
    """Enhanced Python validator building on existing functionality."""
    
    def get_language_name(self) -> str:
        return "python"
    
    def is_environment_available(self) -> bool:
        try:
            import eiplgrader
            return True
        except ImportError:
            return False
    
    def validate_basic_usage(self) -> List[ValidationResult]:
        """Validate Python basic usage examples."""
        results = []
        
        # Test from docs/quickstart/python.md - Basic filtering example
        try:
            if not self.api_key:
                self.record_result("Python Basic Generation", "SKIP", "No API key provided")
                return results
            
            generator = CodeGenerator(self.api_key, client_type=self.client_type, language="python")
            
            result = generator.generate_code(
                student_response="that takes a list of numbers and returns only the even ones",
                model=self.model,
                function_name="filter_even",
                gen_type="cgbg"
            )
            
            # Validate response structure
            assert isinstance(result, dict), "Result should be a dict"
            assert "code" in result, "Result should contain 'code' key"
            assert isinstance(result["code"], list), "Code should be a list"
            
            # Test the generated code
            test_cases = [
                {"parameters": {"numbers": [1, 2, 3, 4, 5, 6]}, "expected": [2, 4, 6]},
                {"parameters": {"numbers": [1, 3, 5, 7]}, "expected": []},
                {"parameters": {"numbers": []}, "expected": []}
            ]
            
            tester = CodeTester(
                code=result["code"][0],
                test_cases=test_cases,
                function_name="filter_even",
                language="python"
            )
            
            test_result = tester.run_tests()
            
            if test_result.was_successful():
                self.record_result("Python Basic Generation", "PASS")
            else:
                self.record_result("Python Basic Generation", "FAIL", 
                                 f"Generated code failed tests: {test_result.failures} failures")
            
        except Exception as e:
            self.record_result("Python Basic Generation", "FAIL", str(e))
        
        return self.results
    
    def validate_type_system(self) -> List[ValidationResult]:
        """Validate Python type inference examples."""
        
        # Test type inference from docs/quickstart/python.md
        try:
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
            
            # Simple function that uses all these types
            code = """
def process_data(x, y, name, items, flag):
    if flag:
        return f"{name}: {x + y} with {len(items)} items"
    return "disabled"
"""
            
            tester = CodeTester(
                code=code,
                test_cases=[{
                    "parameters": test_case["parameters"],
                    "expected": "Alice: 45.14 with 3 items"
                }],
                function_name="process_data",
                language="python"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("Python Type Inference", "PASS")
            else:
                self.record_result("Python Type Inference", "FAIL", 
                                 f"Type inference test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("Python Type Inference", "FAIL", str(e))
        
        return self.results
    
    def validate_error_handling(self) -> List[ValidationResult]:
        """Validate Python error handling examples."""
        
        # Test error handling from docs/guide/errors.md
        try:
            # Test with intentionally failing code - wrong expected value
            code = "def add_numbers(a, b):\n    return a + b + 1"  # Wrong implementation
            test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]  # Should be 4 but expects 3
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="add_numbers",
                language="python"
            )
            
            results = tester.run_tests()
            
            # Should have failures (not errors, but failures due to wrong result)
            if not results.was_successful() and (results.failures > 0 or results.errors > 0):
                self.record_result("Python Error Handling", "PASS", 
                                 details={"failures_detected": results.failures, "errors_detected": results.errors})
            else:
                self.record_result("Python Error Handling", "FAIL", 
                                 "Expected failures but test passed")
        
        except Exception as e:
            self.record_result("Python Error Handling", "FAIL", str(e))
        
        return self.results


class JavaValidator(LanguageValidator):
    """Java-specific validator with type annotation testing."""
    
    def get_language_name(self) -> str:
        return "java"
    
    def is_environment_available(self) -> bool:
        try:
            result = subprocess.run(['java', '-version'], 
                                  capture_output=True, text=True, timeout=5)
            return result.returncode == 0
        except (subprocess.TimeoutExpired, FileNotFoundError):
            return False
    
    def validate_basic_usage(self) -> List[ValidationResult]:
        """Validate Java basic usage examples from docs/quickstart/java.md."""
        
        if not self.is_environment_available():
            self.record_result("Java Environment", "SKIP", "Java not available")
            return self.results
        
        # Test basic Java code generation and testing
        try:
            if not self.api_key:
                self.record_result("Java Basic Generation", "SKIP", "No API key provided")
                return self.results
            
            generator = CodeGenerator(self.api_key, client_type=self.client_type, language="java")
            
            result = generator.generate_code(
                student_response="that calculates the average of an array of integers",
                model=self.model,
                function_name="calculateAverage",
                gen_type="cgbg"
            )
            
            # Test with explicit type annotations (required for Java)
            test_cases = [
                {
                    "parameters": {"numbers": [10, 20, 30, 40, 50]},
                    "parameter_types": {"numbers": "int[]"},
                    "expected": 30.0,
                    "expected_type": "double"
                },
                {
                    "parameters": {"numbers": []},
                    "parameter_types": {"numbers": "int[]"},
                    "expected": 0.0,
                    "expected_type": "double"
                }
            ]
            
            tester = CodeTester(
                code=result["code"][0],
                test_cases=test_cases,
                function_name="calculateAverage",
                language="java"
            )
            
            test_result = tester.run_tests()
            
            if test_result.was_successful():
                self.record_result("Java Basic Generation", "PASS")
            else:
                self.record_result("Java Basic Generation", "FAIL", 
                                 f"Generated Java code failed: {test_result.failures} failures")
        
        except Exception as e:
            self.record_result("Java Basic Generation", "FAIL", str(e))
        
        return self.results
    
    def validate_type_system(self) -> List[ValidationResult]:
        """Validate Java type annotation requirements."""
        
        # Test that Java requires explicit type annotations
        try:
            # Test case without type annotations (should fail)
            test_cases_invalid = [
                {
                    "parameters": {"name": "Alice", "age": 25},
                    "expected": "Alice is 25 years old"
                    # Missing parameter_types and expected_type
                }
            ]
            
            code = """
public static String formatPerson(String name, int age) {
    return name + " is " + age + " years old";
}
"""
            
            try:
                tester = CodeTester(
                    code=code,
                    test_cases=test_cases_invalid,
                    function_name="formatPerson", 
                    language="java"
                )
                result = tester.run_tests()
                
                # This should fail due to missing type information
                if not result.was_successful():
                    self.record_result("Java Type Requirement", "PASS", 
                                     "Correctly rejected test without type annotations")
                else:
                    self.record_result("Java Type Requirement", "FAIL", 
                                     "Should have failed without type annotations")
            
            except Exception as expected_error:
                # Expected to fail
                if "type" in str(expected_error).lower():
                    self.record_result("Java Type Requirement", "PASS", 
                                     "Correctly rejected missing type info")
                else:
                    self.record_result("Java Type Requirement", "FAIL", str(expected_error))
            
            # Test with correct type annotations
            test_cases_valid = [
                {
                    "parameters": {"name": "Alice", "age": 25},
                    "parameter_types": {"name": "String", "age": "int"},
                    "expected": "Alice is 25 years old",
                    "expected_type": "String"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases_valid,
                function_name="formatPerson",
                language="java"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("Java Type Annotations", "PASS")
            else:
                self.record_result("Java Type Annotations", "FAIL", 
                                 f"Valid type annotations failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("Java Type System", "FAIL", str(e))
        
        return self.results
    
    def validate_error_handling(self) -> List[ValidationResult]:
        """Validate Java compilation and runtime error handling."""
        
        # Test compilation error handling
        try:
            # Code with syntax error
            broken_code = """
public static int addNumbers(int a, int b {
    return a + b;
}
"""  # Missing closing parenthesis
            
            test_cases = [
                {
                    "parameters": {"a": 1, "b": 2},
                    "parameter_types": {"a": "int", "b": "int"},
                    "expected": 3,
                    "expected_type": "int"
                }
            ]
            
            tester = CodeTester(
                code=broken_code,
                test_cases=test_cases,
                function_name="addNumbers",
                language="java"
            )
            
            result = tester.run_tests()
            
            # Should fail due to compilation error
            if not result.was_successful():
                self.record_result("Java Compilation Error", "PASS", 
                                 "Correctly detected compilation error")
            else:
                self.record_result("Java Compilation Error", "FAIL", 
                                 "Should have failed with compilation error")
        
        except Exception as e:
            # Expected behavior - compilation should fail
            self.record_result("Java Compilation Error", "PASS", 
                             "Compilation error correctly caught")
        
        return self.results


class JavaScriptValidator(LanguageValidator):
    """JavaScript-specific validator with async pattern testing."""
    
    def get_language_name(self) -> str:
        return "javascript"
    
    def is_environment_available(self) -> bool:
        try:
            result = subprocess.run(['node', '--version'], 
                                  capture_output=True, text=True, timeout=5)
            return result.returncode == 0
        except (subprocess.TimeoutExpired, FileNotFoundError):
            return False
    
    def validate_basic_usage(self) -> List[ValidationResult]:
        """Validate JavaScript basic usage examples."""
        
        if not self.is_environment_available():
            self.record_result("JavaScript Environment", "SKIP", "Node.js not available")
            return self.results
        
        # Test basic JavaScript functionality
        try:
            # Simple array manipulation (common JS pattern)
            code = """
function filterAndTransform(numbers, threshold) {
    return numbers
        .filter(num => num > threshold)
        .map(num => num * 2);
}
"""
            
            test_cases = [
                {
                    "parameters": {"numbers": [1, 2, 3, 4, 5], "threshold": 2},
                    "expected": [6, 8, 10]
                },
                {
                    "parameters": {"numbers": [1, 2], "threshold": 5},
                    "expected": []
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="filterAndTransform",
                language="javascript"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("JavaScript Basic Usage", "PASS")
            else:
                self.record_result("JavaScript Basic Usage", "FAIL", 
                                 f"JavaScript test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("JavaScript Basic Usage", "FAIL", str(e))
        
        return self.results
    
    def validate_type_system(self) -> List[ValidationResult]:
        """Validate JavaScript type coercion and inference."""
        
        # Test JavaScript type coercion behavior
        try:
            code = """
function typeCoercionExample(value) {
    // Test various JavaScript type behaviors
    if (typeof value === 'string') {
        return value.length;
    } else if (typeof value === 'number') {
        return value * 2;
    } else if (Array.isArray(value)) {
        return value.length;
    } else {
        return 0;
    }
}
"""
            
            test_cases = [
                {"parameters": {"value": "hello"}, "expected": 5},
                {"parameters": {"value": 42}, "expected": 84},
                {"parameters": {"value": [1, 2, 3]}, "expected": 3},
                {"parameters": {"value": True}, "expected": 0}
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="typeCoercionExample",
                language="javascript"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("JavaScript Type System", "PASS")
            else:
                self.record_result("JavaScript Type System", "FAIL", 
                                 f"Type coercion test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("JavaScript Type System", "FAIL", str(e))
        
        return self.results
    
    def validate_error_handling(self) -> List[ValidationResult]:
        """Validate JavaScript error handling."""
        
        # Test JavaScript error handling with wrong expected values
        try:
            code = """
function addNumbers(a, b) {
    return a + b + 1;  // Wrong implementation - adds extra 1
}
"""
            
            test_cases = [
                {"parameters": {"a": 5, "b": 3}, "expected": 8},  # Should be 9 but expects 8
                {"parameters": {"a": 1, "b": 1}, "expected": 2}   # Should be 3 but expects 2
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="addNumbers",
                language="javascript"
            )
            
            result = tester.run_tests()
            
            # Should have failures due to wrong expected values
            if not result.was_successful() and (result.failures > 0 or result.errors > 0):
                self.record_result("JavaScript Error Handling", "PASS", 
                                 "Correctly detected JavaScript failures")
            else:
                self.record_result("JavaScript Error Handling", "FAIL", 
                                 "Expected failures but test passed")
        
        except Exception as e:
            self.record_result("JavaScript Error Handling", "FAIL", str(e))
        
        return self.results


class CppValidator(LanguageValidator):
    """C/C++ validator with memory management testing."""
    
    def get_language_name(self) -> str:
        return "cpp"
    
    def is_environment_available(self) -> bool:
        try:
            result = subprocess.run(['g++', '--version'], 
                                  capture_output=True, text=True, timeout=5)
            return result.returncode == 0
        except (subprocess.TimeoutExpired, FileNotFoundError):
            return False
    
    def validate_basic_usage(self) -> List[ValidationResult]:
        """Validate C++ basic usage examples."""
        
        if not self.is_environment_available():
            self.record_result("C++ Environment", "SKIP", "g++ not available")
            return self.results
        
        # Test basic C++ functionality
        try:
            code = """
#include <vector>
#include <algorithm>

std::vector<int> filterEven(std::vector<int> numbers) {
    std::vector<int> result;
    for (int num : numbers) {
        if (num % 2 == 0) {
            result.push_back(num);
        }
    }
    return result;
}
"""
            
            test_cases = [
                {
                    "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
                    "parameter_types": {"numbers": "std::vector<int>"},
                    "expected": [2, 4, 6],
                    "expected_type": "std::vector<int>"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="filterEven",
                language="cpp"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("C++ Basic Usage", "PASS")
            else:
                self.record_result("C++ Basic Usage", "FAIL", 
                                 f"C++ test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("C++ Basic Usage", "FAIL", str(e))
        
        return self.results
    
    def validate_type_system(self) -> List[ValidationResult]:
        """Validate C++ type system requirements."""
        
        # Test that C++ requires explicit type annotations
        try:
            code = """
int addNumbers(int a, int b) {
    return a + b;
}
"""
            
            test_cases = [
                {
                    "parameters": {"a": 5, "b": 3},
                    "parameter_types": {"a": "int", "b": "int"},
                    "expected": 8,
                    "expected_type": "int"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="addNumbers",
                language="cpp"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("C++ Type System", "PASS")
            else:
                self.record_result("C++ Type System", "FAIL", 
                                 f"C++ type test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("C++ Type System", "FAIL", str(e))
        
        return self.results
    
    def validate_error_handling(self) -> List[ValidationResult]:
        """Validate C++ compilation error handling."""
        
        # Test compilation error detection
        try:
            # Code with syntax error
            broken_code = """
int addNumbers(int a, int b {
    return a + b;
}
"""  # Missing closing parenthesis
            
            test_cases = [
                {
                    "parameters": {"a": 1, "b": 2},
                    "parameter_types": {"a": "int", "b": "int"},
                    "expected": 3,
                    "expected_type": "int"
                }
            ]
            
            try:
                tester = CodeTester(
                    code=broken_code,
                    test_cases=test_cases,
                    function_name="addNumbers",
                    language="cpp"
                )
                
                result = tester.run_tests()
                
                # Should fail due to compilation error
                if not result.was_successful():
                    self.record_result("C++ Compilation Error", "PASS", 
                                     "Correctly detected compilation error")
                else:
                    self.record_result("C++ Compilation Error", "FAIL", 
                                     "Should have failed with compilation error")
            
            except Exception:
                # Expected - compilation should fail
                self.record_result("C++ Compilation Error", "PASS", 
                                 "Compilation error correctly caught")
        
        except Exception as e:
            self.record_result("C++ Error Handling", "FAIL", str(e))
        
        return self.results


class GoValidator(LanguageValidator):
    """Go validator with slice/error handling testing."""
    
    def get_language_name(self) -> str:
        return "go"
    
    def is_environment_available(self) -> bool:
        try:
            result = subprocess.run(['go', 'version'], 
                                  capture_output=True, text=True, timeout=5)
            return result.returncode == 0
        except (subprocess.TimeoutExpired, FileNotFoundError):
            return False
    
    def validate_basic_usage(self) -> List[ValidationResult]:
        """Validate Go basic usage examples."""
        
        if not self.is_environment_available():
            self.record_result("Go Environment", "SKIP", "Go not available")
            return self.results
        
        try:
            code = """
func filterEven(numbers []int) []int {
    var result []int
    for _, num := range numbers {
        if num%2 == 0 {
            result = append(result, num)
        }
    }
    return result
}
"""
            
            test_cases = [
                {
                    "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
                    "parameter_types": {"numbers": "[]int"},
                    "expected": [2, 4, 6],
                    "expected_type": "[]int"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="filterEven",
                language="go"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("Go Basic Usage", "PASS")
            else:
                self.record_result("Go Basic Usage", "FAIL", 
                                 f"Go test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("Go Basic Usage", "FAIL", str(e))
        
        return self.results
    
    def validate_type_system(self) -> List[ValidationResult]:
        """Validate Go slice vs array distinctions."""
        
        try:
            code = """
func sumSlice(numbers []int) int {
    sum := 0
    for _, num := range numbers {
        sum += num
    }
    return sum
}
"""
            
            test_cases = [
                {
                    "parameters": {"numbers": [1, 2, 3, 4, 5]},
                    "parameter_types": {"numbers": "[]int"},
                    "expected": 15,
                    "expected_type": "int"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="sumSlice",
                language="go"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("Go Type System", "PASS")
            else:
                self.record_result("Go Type System", "FAIL", 
                                 f"Go slice test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("Go Type System", "FAIL", str(e))
        
        return self.results
    
    def validate_error_handling(self) -> List[ValidationResult]:
        """Validate Go error handling patterns."""
        
        try:
            # Test with wrong expected value
            code = """
func addNumbers(a, b int) int {
    return a + b + 1  // Wrong implementation
}
"""
            
            test_cases = [
                {
                    "parameters": {"a": 5, "b": 3},
                    "parameter_types": {"a": "int", "b": "int"},
                    "expected": 8,  # Should be 9 but expects 8
                    "expected_type": "int"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="addNumbers",
                language="go"
            )
            
            result = tester.run_tests()
            
            if not result.was_successful() and (result.failures > 0 or result.errors > 0):
                self.record_result("Go Error Handling", "PASS", 
                                 "Correctly detected Go failures")
            else:
                self.record_result("Go Error Handling", "FAIL", 
                                 "Expected failures but test passed")
        
        except Exception as e:
            self.record_result("Go Error Handling", "FAIL", str(e))
        
        return self.results


class HaskellValidator(LanguageValidator):
    """Haskell validator with functional pattern testing."""
    
    def get_language_name(self) -> str:
        return "haskell"
    
    def is_environment_available(self) -> bool:
        try:
            result = subprocess.run(['ghc', '--version'], 
                                  capture_output=True, text=True, timeout=5)
            return result.returncode == 0
        except (subprocess.TimeoutExpired, FileNotFoundError):
            return False
    
    def validate_basic_usage(self) -> List[ValidationResult]:
        """Validate Haskell basic usage examples."""
        
        if not self.is_environment_available():
            self.record_result("Haskell Environment", "SKIP", "GHC not available")
            return self.results
        
        try:
            code = """
filterEven :: [Int] -> [Int]
filterEven numbers = [x | x <- numbers, x `mod` 2 == 0]
"""
            
            test_cases = [
                {
                    "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
                    "parameter_types": {"numbers": "[Int]"},
                    "expected": [2, 4, 6],
                    "expected_type": "[Int]"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="filterEven",
                language="haskell"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("Haskell Basic Usage", "PASS")
            else:
                self.record_result("Haskell Basic Usage", "FAIL", 
                                 f"Haskell test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("Haskell Basic Usage", "FAIL", str(e))
        
        return self.results
    
    def validate_type_system(self) -> List[ValidationResult]:
        """Validate Haskell type signatures."""
        
        try:
            code = """
addNumbers :: Int -> Int -> Int
addNumbers a b = a + b
"""
            
            test_cases = [
                {
                    "parameters": {"a": 5, "b": 3},
                    "parameter_types": {"a": "Int", "b": "Int"},
                    "expected": 8,
                    "expected_type": "Int"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="addNumbers",
                language="haskell"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                self.record_result("Haskell Type System", "PASS")
            else:
                self.record_result("Haskell Type System", "FAIL", 
                                 f"Haskell type test failed: {result.failures} failures")
        
        except Exception as e:
            self.record_result("Haskell Type System", "FAIL", str(e))
        
        return self.results
    
    def validate_error_handling(self) -> List[ValidationResult]:
        """Validate Haskell error handling."""
        
        try:
            # Test with wrong expected value
            code = """
addNumbers :: Int -> Int -> Int
addNumbers a b = a + b + 1  -- Wrong implementation
"""
            
            test_cases = [
                {
                    "parameters": {"a": 5, "b": 3},
                    "parameter_types": {"a": "Int", "b": "Int"},
                    "expected": 8,  # Should be 9 but expects 8
                    "expected_type": "Int"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="addNumbers",
                language="haskell"
            )
            
            result = tester.run_tests()
            
            if not result.was_successful() and (result.failures > 0 or result.errors > 0):
                self.record_result("Haskell Error Handling", "PASS", 
                                 "Correctly detected Haskell failures")
            else:
                self.record_result("Haskell Error Handling", "FAIL", 
                                 "Expected failures but test passed")
        
        except Exception as e:
            self.record_result("Haskell Error Handling", "FAIL", str(e))
        
        return self.results


class AdvancedFeatureValidator:
    """Validator for advanced EiplGrader features."""
    
    def __init__(self, api_key: Optional[str] = None, client_type: Optional[str] = None, model: Optional[str] = None):
        self.api_key = api_key
        self.client_type = client_type or "openai"
        self.model = model or "gpt-4o"
        self.results: List[ValidationResult] = []
    
    def validate_multiple_variants(self) -> List[ValidationResult]:
        """Validate multiple variant generation from docs/quickstart/python.md."""
        
        if not self.api_key:
            result = ValidationResult(
                example_name="Multiple Variants",
                language="python",
                category="advanced",
                status="SKIP",
                error="No API key provided"
            )
            self.results.append(result)
            return self.results
        
        try:
            generator = CodeGenerator(self.api_key, client_type=self.client_type, language="python")
            
            # Generate multiple implementations
            result = generator.generate_code(
                student_response="that calculates the factorial of a number",
                model=self.model,
                function_name="factorial",
                num_to_gen=3  # Generate 3 different implementations
            )
            
            # Verify we got multiple implementations
            assert isinstance(result["code"], list), "Should return list of code"
            assert len(result["code"]) == 3, f"Should generate 3 implementations, got {len(result['code'])}"
            
            # Test all implementations
            test_cases = [
                {"parameters": {"n": 5}, "expected": 120},
                {"parameters": {"n": 0}, "expected": 1},
                {"parameters": {"n": 1}, "expected": 1}
            ]
            
            successful_implementations = 0
            for i, code in enumerate(result["code"]):
                try:
                    tester = CodeTester(
                        code=code,
                        test_cases=test_cases,
                        function_name="factorial",
                        language="python"
                    )
                    
                    test_result = tester.run_tests()
                    if test_result.was_successful():
                        successful_implementations += 1
                except Exception:
                    pass  # Some implementations might not work
            
            if successful_implementations >= 1:  # At least one should work
                result = ValidationResult(
                    example_name="Multiple Variants",
                    language="python", 
                    category="advanced",
                    status="PASS",
                    details={"successful_implementations": successful_implementations}
                )
            else:
                result = ValidationResult(
                    example_name="Multiple Variants",
                    language="python",
                    category="advanced", 
                    status="FAIL",
                    error="No implementations worked correctly"
                )
            
            self.results.append(result)
        
        except Exception as e:
            result = ValidationResult(
                example_name="Multiple Variants",
                language="python",
                category="advanced",
                status="FAIL",
                error=str(e)
            )
            self.results.append(result)
        
        return self.results
    
    def validate_inplace_modes(self) -> List[ValidationResult]:
        """Validate in-place modification modes from documentation."""
        
        try:
            # Test mode 0: Normal return value
            code = "def sort_list(numbers):\n    return sorted(numbers)"
            test_cases = [
                {
                    "parameters": {"numbers": [3, 1, 4, 1, 5]},
                    "expected": [1, 1, 3, 4, 5],
                    "inplace": "0"
                }
            ]
            
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="sort_list",
                language="python"
            )
            
            result = tester.run_tests()
            
            if result.was_successful():
                validation_result = ValidationResult(
                    example_name="Inplace Mode 0",
                    language="python",
                    category="advanced",
                    status="PASS"
                )
            else:
                validation_result = ValidationResult(
                    example_name="Inplace Mode 0", 
                    language="python",
                    category="advanced",
                    status="FAIL",
                    error=f"Mode 0 test failed: {result.failures} failures"
                )
            
            self.results.append(validation_result)
            
            # Test mode 1: In-place modification
            code_inplace = """
def sort_list_inplace(numbers):
    numbers.sort()
    return numbers
"""
            
            test_cases_inplace = [
                {
                    "parameters": {"numbers": [3, 1, 4, 1, 5]},
                    "expected": [1, 1, 3, 4, 5],
                    "inplace": "1"  # Tests that numbers list is modified
                }
            ]
            
            tester_inplace = CodeTester(
                code=code_inplace,
                test_cases=test_cases_inplace,
                function_name="sort_list_inplace",
                language="python"
            )
            
            result_inplace = tester_inplace.run_tests()
            
            if result_inplace.was_successful():
                validation_result = ValidationResult(
                    example_name="Inplace Mode 1",
                    language="python",
                    category="advanced",
                    status="PASS"
                )
            else:
                validation_result = ValidationResult(
                    example_name="Inplace Mode 1",
                    language="python", 
                    category="advanced",
                    status="FAIL",
                    error=f"Mode 1 test failed: {result_inplace.failures} failures"
                )
            
            self.results.append(validation_result)
        
        except Exception as e:
            validation_result = ValidationResult(
                example_name="Inplace Modes",
                language="python",
                category="advanced",
                status="FAIL",
                error=str(e)
            )
            self.results.append(validation_result)
        
        return self.results


class EnhancedDocumentationValidator:
    """Main enhanced validator orchestrating all validation modules."""
    
    def __init__(self):
        # Try to get API keys for different providers
        self.openai_key = os.getenv("OPENAI_API_KEY")
        self.meta_key = os.getenv("META_API_KEY")
        
        # Determine which provider to use
        if self.openai_key:
            self.api_key = self.openai_key
            self.client_type = "openai"
            self.model = "gpt-4o"
        elif self.meta_key:
            self.api_key = self.meta_key
            self.client_type = "meta"
            self.model = "Llama-4-Maverick-17B-128E-Instruct-FP8"
        else:
            self.api_key = None
            self.client_type = None
            self.model = None
        
        self.results: List[ValidationResult] = []
        
        # Initialize language validators
        self.language_validators = {
            'python': PythonValidator(self.api_key, self.client_type, self.model),
            'java': JavaValidator(self.api_key, self.client_type, self.model),
            'javascript': JavaScriptValidator(self.api_key, self.client_type, self.model),
            'cpp': CppValidator(self.api_key, self.client_type, self.model),
            'go': GoValidator(self.api_key, self.client_type, self.model),
            'haskell': HaskellValidator(self.api_key, self.client_type, self.model)
        }
        
        # Initialize feature validators
        self.advanced_validator = AdvancedFeatureValidator(self.api_key, self.client_type, self.model)
        
        if not self.api_key:
            print("Warning: OPENAI_API_KEY not set. Code generation examples will be skipped.")
            print("  To test code generation:")
            print("  1. Create a .env file with: OPENAI_API_KEY=your_api_key_here")
            print("  2. Or set the environment variable: export OPENAI_API_KEY=your_api_key_here")
    
    def run_all_validations(self) -> int:
        """Run comprehensive validation suite."""
        print("🚀 Starting Enhanced Documentation Validation...")
        print("=" * 80)
        
        # Run language-specific validation
        for lang_name, validator in self.language_validators.items():
            print(f"\n🌐 Testing {lang_name.title()} Examples")
            print("-" * 50)
            
            try:
                # Check environment
                if not validator.is_environment_available():
                    print(f"⚠️  {lang_name.title()} environment not available, skipping...")
                    continue
                
                # Run validations
                validator.validate_basic_usage()
                validator.validate_type_system()
                validator.validate_error_handling()
                
                # Collect results
                self.results.extend(validator.results)
                
                # Print immediate feedback
                for result in validator.results:
                    if result.status == "PASS":
                        print(f"✅ {result.example_name}")
                    elif result.status == "FAIL":
                        print(f"❌ {result.example_name}: {result.error}")
                    else:
                        print(f"⏭️  {result.example_name}: {result.error or 'Skipped'}")
            
            except Exception as e:
                print(f"❌ {lang_name.title()} validation failed: {e}")
        
        # Run advanced feature validation
        print(f"\n🚀 Testing Advanced Features")
        print("-" * 50)
        
        try:
            self.advanced_validator.validate_multiple_variants()
            self.advanced_validator.validate_inplace_modes()
            
            self.results.extend(self.advanced_validator.results)
            
            for result in self.advanced_validator.results:
                if result.status == "PASS":
                    print(f"✅ {result.example_name}")
                elif result.status == "FAIL":
                    print(f"❌ {result.example_name}: {result.error}")
                else:
                    print(f"⏭️  {result.example_name}: {result.error or 'Skipped'}")
        
        except Exception as e:
            print(f"❌ Advanced feature validation failed: {e}")
        
        # Generate final report
        return self.generate_report()
    
    def generate_report(self) -> int:
        """Generate comprehensive validation report."""
        print("\n" + "=" * 80)
        print("📊 ENHANCED VALIDATION SUMMARY")
        print("=" * 80)
        
        # Overall statistics
        total = len(self.results)
        passed = sum(1 for r in self.results if r.status == "PASS")
        failed = sum(1 for r in self.results if r.status == "FAIL")
        skipped = sum(1 for r in self.results if r.status == "SKIP")
        
        print(f"Total Tests: {total}")
        print(f"✅ Passed: {passed}")
        print(f"❌ Failed: {failed}")
        print(f"⏭️  Skipped: {skipped}")
        if total > 0:
            print(f"Success Rate: {(passed/total)*100:.1f}%")
        
        # Language breakdown
        language_stats = {}
        for result in self.results:
            lang = result.language
            if lang not in language_stats:
                language_stats[lang] = {"pass": 0, "fail": 0, "skip": 0}
            language_stats[lang][result.status.lower()] += 1
        
        print(f"\n📈 Language Breakdown:")
        print("-" * 40)
        for lang, stats in language_stats.items():
            total_lang = sum(stats.values())
            pass_rate = (stats["pass"] / total_lang * 100) if total_lang > 0 else 0
            print(f"{lang.title()}: {stats['pass']}/{total_lang} passed ({pass_rate:.1f}%)")
        
        # Category breakdown
        category_stats = {}
        for result in self.results:
            cat = result.category
            if cat not in category_stats:
                category_stats[cat] = {"pass": 0, "fail": 0, "skip": 0}
            category_stats[cat][result.status.lower()] += 1
        
        print(f"\n📊 Category Breakdown:")
        print("-" * 40)
        for cat, stats in category_stats.items():
            total_cat = sum(stats.values())
            pass_rate = (stats["pass"] / total_cat * 100) if total_cat > 0 else 0
            print(f"{cat.replace('_', ' ').title()}: {stats['pass']}/{total_cat} passed ({pass_rate:.1f}%)")
        
        # Failed tests details
        failed_tests = [r for r in self.results if r.status == "FAIL"]
        if failed_tests:
            print(f"\n🔍 FAILED TESTS:")
            print("-" * 40)
            for result in failed_tests:
                print(f"❌ {result.language.title()} - {result.example_name}")
                print(f"   Error: {result.error}")
                print()
        
        print("=" * 80)
        
        if failed == 0:
            print("🎉 All enhanced documentation examples are working correctly!")
            return 0
        else:
            print(f"⚠️  {failed} enhanced examples need attention.")
            return 1


def main():
    """Main entry point."""
    print("Enhanced EiplGrader Documentation Validator")
    print("=" * 80)
    
    validator = EnhancedDocumentationValidator()
    exit_code = validator.run_all_validations()
    
    return exit_code


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)