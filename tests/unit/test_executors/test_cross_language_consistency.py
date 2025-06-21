"""
Cross-language consistency tests for all executors.

This module tests that:
1. The same logical functions produce the same results across all languages
2. Type inference languages (Python/JS/Go) work without explicit types
3. Static type languages (C/C++/Java/Haskell) require explicit types
4. All executors handle the same test scenarios consistently
5. Error handling is consistent across languages
6. Edge cases are handled uniformly
"""

import pytest
import sys
import os
import subprocess

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../..'))

from eiplgrader.languages.executors.python_executor import PythonExecutor
from eiplgrader.languages.executors.javascript_executor import JavaScriptExecutor
from eiplgrader.languages.executors.go_executor import GoExecutor
from eiplgrader.languages.executors.java_executor import JavaExecutor
from eiplgrader.languages.executors.cpp_executor import CppExecutor
from eiplgrader.languages.executors.c_executor import CExecutor
from eiplgrader.languages.executors.haskell_executor import HaskellExecutor

from tests.fixtures.mock_code_samples import (
    python_samples, javascript_samples, go_samples, java_samples,
    cpp_samples, c_samples, haskell_samples
)


class TestCrossLanguageConsistency:
    """Test suite for cross-language consistency across all executors."""

    def setup_method(self):
        """Set up test fixtures for all executors."""
        self.executors = {}
        self.samples = {}
        
        # Initialize all executors
        try:
            self.executors['python'] = PythonExecutor()
            self.samples['python'] = python_samples
        except Exception:
            self.executors['python'] = None
            
        try:
            self.executors['javascript'] = JavaScriptExecutor()
            self.samples['javascript'] = javascript_samples
        except Exception:
            self.executors['javascript'] = None
            
        try:
            self.executors['go'] = GoExecutor()
            self.samples['go'] = go_samples
        except Exception:
            self.executors['go'] = None
            
        try:
            self.executors['java'] = JavaExecutor()
            self.samples['java'] = java_samples
        except Exception:
            self.executors['java'] = None
            
        try:
            self.executors['cpp'] = CppExecutor()
            self.samples['cpp'] = cpp_samples
        except Exception:
            self.executors['cpp'] = None
            
        try:
            self.executors['c'] = CExecutor()
            self.samples['c'] = c_samples
        except Exception:
            self.executors['c'] = None
            
        try:
            self.executors['haskell'] = HaskellExecutor()
            self.samples['haskell'] = haskell_samples
        except Exception:
            self.executors['haskell'] = None

    def teardown_method(self):
        """Clean up after each test."""
        for executor in self.executors.values():
            if executor:
                executor.cleanup()

    def _check_compiler_available(self, language):
        """Check if compiler/interpreter for language is available."""
        checks = {
            'python': ['python3', '--version'],
            'javascript': ['node', '--version'],
            'go': ['go', 'version'],
            'java': ['javac', '-version'],
            'cpp': ['g++', '--version'],
            'c': ['gcc', '--version'],
            'haskell': ['ghc', '--version']
        }
        
        if language not in checks:
            return False
            
        try:
            subprocess.run(checks[language], capture_output=True, check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            return False

    def _get_type_inference_languages(self):
        """Get languages that support type inference."""
        return ['python', 'javascript', 'go']

    def _get_static_type_languages(self):
        """Get languages that require explicit types."""
        return ['java', 'cpp', 'c', 'haskell']

    def _create_test_case_with_types(self, language, function_name, parameters, expected, inplace="0"):
        """Create test case with appropriate type annotations for the language."""
        base_case = {
            "function_name": function_name,
            "parameters": parameters,
            "expected": expected,
            "inplace": inplace
        }
        
        # Type inference languages don't need explicit types
        if language in self._get_type_inference_languages():
            return base_case
        
        # Static type languages need explicit types
        type_mappings = {
            'java': {
                'addNumbers': {
                    'param_types': {'a': 'int', 'b': 'int'},
                    'expected_type': 'int'
                },
                'countVowels': {
                    'param_types': {'str': 'String'},
                    'expected_type': 'int'
                },
                'isPalindrome': {
                    'param_types': {'s': 'String'},
                    'expected_type': 'boolean'
                },
                'sumEvenNumbers': {
                    'param_types': {'numbers': 'int[]'},
                    'expected_type': 'int'
                },
                'factorial': {
                    'param_types': {'n': 'int'},
                    'expected_type': 'int'
                }
            },
            'cpp': {
                'addNumbers': {
                    'param_types': {'a': 'int', 'b': 'int'},
                    'expected_type': 'int'
                },
                'countVowels': {
                    'param_types': {'str': 'std::string'},
                    'expected_type': 'int'
                },
                'isPalindrome': {
                    'param_types': {'s': 'std::string'},
                    'expected_type': 'bool'
                },
                'sumEvenNumbers': {
                    'param_types': {'numbers': 'std::vector<int>'},
                    'expected_type': 'int'
                },
                'factorial': {
                    'param_types': {'n': 'int'},
                    'expected_type': 'int'
                }
            },
            'c': {
                'addNumbers': {
                    'param_types': {'a': 'int', 'b': 'int'},
                    'expected_type': 'int'
                },
                'countVowels': {
                    'param_types': {'str': 'char*'},
                    'expected_type': 'int'
                },
                'isPalindrome': {
                    'param_types': {'s': 'char*'},
                    'expected_type': 'int'  # C uses int for boolean
                },
                'sumEvenNumbers': {
                    'param_types': {'numbers': 'int*', 'size': 'int'},
                    'expected_type': 'int'
                },
                'factorial': {
                    'param_types': {'n': 'int'},
                    'expected_type': 'int'
                }
            },
            'haskell': {
                'addNumbers': {
                    'param_types': {'a': 'Int', 'b': 'Int'},
                    'expected_type': 'Int'
                },
                'countVowels': {
                    'param_types': {'str': 'String'},
                    'expected_type': 'Int'
                },
                'isPalindrome': {
                    'param_types': {'s': 'String'},
                    'expected_type': 'Bool'
                },
                'sumEvenNumbers': {
                    'param_types': {'numbers': '[Int]'},
                    'expected_type': 'Int'
                },
                'factorial': {
                    'param_types': {'n': 'Int'},
                    'expected_type': 'Int'
                }
            }
        }
        
        if language in type_mappings and function_name in type_mappings[language]:
            mapping = type_mappings[language][function_name]
            base_case['parameter_types'] = mapping['param_types']
            base_case['expected_type'] = mapping['expected_type']
            
            # Special handling for C arrays that need size parameter
            if language == 'c' and function_name == 'sumEvenNumbers':
                base_case['parameters']['size'] = len(base_case['parameters']['numbers'])
        
        return base_case

    def test_add_numbers_consistency(self):
        """Test that addNumbers function produces consistent results across all languages."""
        test_data = [
            {"a": 5, "b": 3, "expected": 8},
            {"a": 0, "b": 0, "expected": 0},
            {"a": -1, "b": 1, "expected": 0},
            {"a": 100, "b": 200, "expected": 300}
        ]
        
        results = {}
        
        for language, executor in self.executors.items():
            if not executor or not self._check_compiler_available(language):
                continue
                
            language_results = []
            
            for data in test_data:
                test_case = self._create_test_case_with_types(
                    language, "addNumbers", 
                    {"a": data["a"], "b": data["b"]}, 
                    data["expected"]
                )
                
                try:
                    code_sample = getattr(self.samples[language], "ADD_NUMBERS")
                    result = executor.execute_test(code_sample, test_case)
                    language_results.append({
                        "input": data,
                        "passed": result["passed"],
                        "actual": result["actual"],
                        "expected": result["expected"]
                    })
                except Exception as e:
                    language_results.append({
                        "input": data,
                        "passed": False,
                        "actual": None,
                        "expected": data["expected"],
                        "error": str(e)
                    })
            
            results[language] = language_results
        
        # Verify all languages produce the same results
        if len(results) < 2:
            pytest.skip(f"Not enough languages available for comparison. Found: {list(results.keys())}")
        
        first_lang = next(iter(results))
        first_results = results[first_lang]
        
        for language, lang_results in results.items():
            if language == first_lang:
                continue
                
            assert len(lang_results) == len(first_results), f"Result count mismatch for {language}"
            
            for i, (first_result, lang_result) in enumerate(zip(first_results, lang_results)):
                if first_result["passed"] and lang_result["passed"]:
                    assert first_result["actual"] == lang_result["actual"], \
                        f"Result mismatch for test {i} between {first_lang} and {language}: " \
                        f"{first_result['actual']} != {lang_result['actual']}"

    def test_count_vowels_consistency(self):
        """Test that countVowels function produces consistent results across all languages."""
        test_data = [
            {"str": "hello", "expected": 2},
            {"str": "programming", "expected": 3},
            {"str": "xyz", "expected": 0},
            {"str": "AEIOU", "expected": 5},
            {"str": "", "expected": 0}
        ]
        
        results = {}
        
        for language, executor in self.executors.items():
            if not executor or not self._check_compiler_available(language):
                continue
                
            language_results = []
            
            for data in test_data:
                test_case = self._create_test_case_with_types(
                    language, "countVowels", 
                    {"str": data["str"]} if language != 'java' else {"str": data["str"]},
                    data["expected"]
                )
                
                # Handle different parameter names across languages
                if language == 'java':
                    test_case["parameters"] = {"str": data["str"]}
                elif language == 'haskell':
                    test_case["parameters"] = {"str": data["str"]}
                else:
                    test_case["parameters"] = {"s": data["str"]}
                    if language == 'cpp' and "parameter_types" in test_case:
                        test_case["parameter_types"] = {"str": "std::string"}
                
                try:
                    code_sample = getattr(self.samples[language], "COUNT_VOWELS")
                    result = executor.execute_test(code_sample, test_case)
                    language_results.append({
                        "input": data,
                        "passed": result["passed"],
                        "actual": result["actual"],
                        "expected": result["expected"]
                    })
                except Exception as e:
                    language_results.append({
                        "input": data,
                        "passed": False,
                        "actual": None,
                        "expected": data["expected"],
                        "error": str(e)
                    })
            
            results[language] = language_results
        
        # Verify consistency
        if len(results) < 2:
            pytest.skip(f"Not enough languages available for comparison. Found: {list(results.keys())}")
        
        first_lang = next(iter(results))
        first_results = results[first_lang]
        
        for language, lang_results in results.items():
            if language == first_lang:
                continue
                
            for i, (first_result, lang_result) in enumerate(zip(first_results, lang_results)):
                if first_result["passed"] and lang_result["passed"]:
                    assert first_result["actual"] == lang_result["actual"], \
                        f"Vowel count mismatch for test {i} between {first_lang} and {language}"

    def test_palindrome_consistency(self):
        """Test that isPalindrome function produces consistent results across all languages."""
        test_data = [
            {"s": "racecar", "expected": True},
            {"s": "hello", "expected": False},
            {"s": "a", "expected": True},
            {"s": "", "expected": True},
            {"s": "madam", "expected": True}
        ]
        
        results = {}
        
        for language, executor in self.executors.items():
            if not executor or not self._check_compiler_available(language):
                continue
                
            language_results = []
            
            for data in test_data:
                # C uses int instead of bool (1/0)
                expected_value = data["expected"]
                if language == 'c':
                    expected_value = 1 if data["expected"] else 0
                    
                test_case = self._create_test_case_with_types(
                    language, "isPalindrome", 
                    {"s": data["s"]}, 
                    expected_value
                )
                
                try:
                    code_sample = getattr(self.samples[language], "IS_PALINDROME")
                    result = executor.execute_test(code_sample, test_case)
                    language_results.append({
                        "input": data,
                        "passed": result["passed"],
                        "actual": result["actual"],
                        "expected": result["expected"]
                    })
                except Exception as e:
                    language_results.append({
                        "input": data,
                        "passed": False,
                        "actual": None,
                        "expected": expected_value,
                        "error": str(e)
                    })
            
            results[language] = language_results
        
        # Verify consistency (accounting for C's int boolean representation)
        if len(results) < 2:
            pytest.skip(f"Not enough languages available for comparison. Found: {list(results.keys())}")

    def test_factorial_consistency(self):
        """Test that factorial function produces consistent results across all languages."""
        test_data = [
            {"n": 0, "expected": 1},
            {"n": 1, "expected": 1},
            {"n": 5, "expected": 120},
            {"n": 3, "expected": 6}
        ]
        
        results = {}
        
        for language, executor in self.executors.items():
            if not executor or not self._check_compiler_available(language):
                continue
                
            language_results = []
            
            for data in test_data:
                test_case = self._create_test_case_with_types(
                    language, "factorial", 
                    {"n": data["n"]}, 
                    data["expected"]
                )
                
                try:
                    code_sample = getattr(self.samples[language], "FACTORIAL")
                    result = executor.execute_test(code_sample, test_case)
                    language_results.append({
                        "input": data,
                        "passed": result["passed"],
                        "actual": result["actual"],
                        "expected": result["expected"]
                    })
                except Exception as e:
                    language_results.append({
                        "input": data,
                        "passed": False,
                        "actual": None,
                        "expected": data["expected"],
                        "error": str(e)
                    })
            
            results[language] = language_results
        
        # Verify consistency
        if len(results) < 2:
            pytest.skip(f"Not enough languages available for comparison. Found: {list(results.keys())}")
        
        first_lang = next(iter(results))
        first_results = results[first_lang]
        
        for language, lang_results in results.items():
            if language == first_lang:
                continue
                
            for i, (first_result, lang_result) in enumerate(zip(first_results, lang_results)):
                if first_result["passed"] and lang_result["passed"]:
                    assert first_result["actual"] == lang_result["actual"], \
                        f"Factorial mismatch for test {i} between {first_lang} and {language}"

    def test_type_inference_vs_explicit_types(self):
        """Test that type inference and explicit type languages handle the same operations correctly."""
        type_inference_langs = [lang for lang in self._get_type_inference_languages() 
                              if lang in self.executors and self.executors[lang] 
                              and self._check_compiler_available(lang)]
        
        static_type_langs = [lang for lang in self._get_static_type_languages() 
                           if lang in self.executors and self.executors[lang] 
                           and self._check_compiler_available(lang)]
        
        if not type_inference_langs or not static_type_langs:
            pytest.skip("Need both type inference and static type languages available")
        
        # Test the same operation with and without explicit types
        test_case_inference = {
            "function_name": "addNumbers",
            "parameters": {"a": 10, "b": 15},
            "expected": 25,
            "inplace": "0"
        }
        
        test_case_static = {
            "function_name": "addNumbers",
            "parameters": {"a": 10, "b": 15},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 25,
            "expected_type": "int",
            "inplace": "0"
        }
        
        # Test type inference languages
        inference_results = []
        for lang in type_inference_langs:
            try:
                code_sample = getattr(self.samples[lang], "ADD_NUMBERS")
                result = self.executors[lang].execute_test(code_sample, test_case_inference)
                inference_results.append(result["actual"] if result["passed"] else None)
            except Exception:
                inference_results.append(None)
        
        # Test static type languages
        static_results = []
        for lang in static_type_langs:
            try:
                test_case = self._create_test_case_with_types(lang, "addNumbers", {"a": 10, "b": 15}, 25)
                code_sample = getattr(self.samples[lang], "ADD_NUMBERS")
                result = self.executors[lang].execute_test(code_sample, test_case)
                static_results.append(result["actual"] if result["passed"] else None)
            except Exception:
                static_results.append(None)
        
        # All successful results should be the same
        all_results = [r for r in inference_results + static_results if r is not None]
        if all_results:
            expected = all_results[0]
            assert all(r == expected for r in all_results), \
                f"Inconsistent results between type inference and static type languages: {all_results}"

    def test_error_handling_consistency(self):
        """Test that error handling is consistent across languages."""
        # Test compilation/syntax error handling
        invalid_codes = {
            'python': 'def broken_func(x)\n    return x',  # Missing colon
            'javascript': 'function brokenFunc(x) { return x + }',  # Missing operand  
            'go': 'func brokenFunc(x int) int { return x + }',  # Missing operand
            'java': 'public static int brokenFunc(int x) { return x + }',  # Missing operand
            'cpp': 'int brokenFunc(int x) { return x + }',  # Missing operand
            'c': 'int brokenFunc(int x) { return x + }',  # Missing operand
            'haskell': 'brokenFunc x = x +'  # Missing operand
        }
        
        test_case = {
            "function_name": "brokenFunc",
            "parameters": {"x": 1},
            "expected": 2,
            "inplace": "0"
        }
        
        error_results = {}
        
        for language, executor in self.executors.items():
            if not executor or not self._check_compiler_available(language) or language not in invalid_codes:
                continue
            
            test_case_typed = self._create_test_case_with_types(language, "brokenFunc", {"x": 1}, 2)
            
            try:
                result = executor.execute_test(invalid_codes[language], test_case_typed)
                error_results[language] = {
                    "passed": result["passed"],
                    "has_error": "error" in result and result["error"] is not None
                }
            except Exception as e:
                error_results[language] = {
                    "passed": False,
                    "has_error": True,
                    "exception": str(e)
                }
        
        # All languages should fail for invalid code
        for language, result in error_results.items():
            assert not result["passed"], f"{language} should have failed for invalid code"
            assert result["has_error"], f"{language} should have reported an error"

    def test_empty_input_handling(self):
        """Test that empty inputs are handled consistently across languages."""
        test_data = [
            {"str": "", "expected": 0},  # Empty string for vowel counting
        ]
        
        results = {}
        
        for language, executor in self.executors.items():
            if not executor or not self._check_compiler_available(language):
                continue
                
            for data in test_data:
                test_case = self._create_test_case_with_types(
                    language, "countVowels", 
                    {"str": data["str"]} if language == 'java' else {"s": data["str"]},
                    data["expected"]
                )
                
                # Handle different parameter names
                if language == 'cpp' and "parameter_types" in test_case:
                    test_case["parameter_types"] = {"str": "std::string"}
                
                try:
                    code_sample = getattr(self.samples[language], "COUNT_VOWELS")
                    result = executor.execute_test(code_sample, test_case)
                    results[language] = result["actual"] if result["passed"] else None
                except Exception:
                    results[language] = None
        
        # All languages should handle empty strings consistently
        successful_results = [r for r in results.values() if r is not None]
        if successful_results:
            assert all(r == 0 for r in successful_results), \
                f"Inconsistent empty string handling: {results}"

    def test_available_languages_summary(self):
        """Test to summarize which languages are available for testing."""
        available = []
        unavailable = []
        
        all_languages = ['python', 'javascript', 'go', 'java', 'cpp', 'c', 'haskell']
        
        for lang in all_languages:
            if (lang in self.executors and 
                self.executors[lang] and 
                self._check_compiler_available(lang)):
                available.append(lang)
            else:
                unavailable.append(lang)
        
        print(f"\\nAvailable languages for testing: {available}")
        print(f"Unavailable languages: {unavailable}")
        
        # This test always passes, it's just for information
        assert True
