"""Tests for error propagation from compilation/execution failures."""

import pytest
import tempfile
import os
import shutil
import subprocess
import json
from unittest.mock import Mock, patch, MagicMock
from typing import Dict, Any

from eiplgrader.languages.executors.base_executors import (
    CompiledLanguageExecutor,
    InterpretedLanguageExecutor,
)
from eiplgrader.languages.registry import LanguageRegistry
from eiplgrader.languages.base import LanguageAdapter, LanguageConfig


class MockCompiledExecutor(CompiledLanguageExecutor):
    """Mock compiled executor for testing."""

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        return code

    def cleanup(self) -> None:
        pass


class MockInterpretedExecutor(InterpretedLanguageExecutor):
    """Mock interpreted executor for testing."""

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        return code

    def cleanup(self) -> None:
        pass


class TestCompilationErrorPropagation:
    """Test error propagation from compilation failures."""

    @patch("subprocess.run")
    def test_syntax_error_propagation(self, mock_run):
        """Test that compilation syntax errors are properly propagated."""
        mock_run.return_value = Mock(
            returncode=1,
            stderr="test.c:5:12: error: expected ';' before 'return'",
            stdout="",
        )

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="invalid syntax"):
                test_case = {"parameters": {"x": 5}, "expected": 10}
                result = executor.execute_test("invalid code", test_case)

                # Error should be propagated with details
                assert result["passed"] is False
                assert "Compilation failed:" in result["error"]
                assert "expected ';'" in result["error"]
                assert result["actual"] is None
                assert result["expected"] == 10

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_missing_header_error_propagation(self, mock_run):
        """Test that missing header errors are properly propagated."""
        mock_run.return_value = Mock(
            returncode=1,
            stderr="test.c:1:10: fatal error: nonexistent.h: No such file or directory",
            stdout="",
        )

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(
                executor, "prepare_code", return_value="#include <nonexistent.h>"
            ):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("missing header", test_case)

                assert result["passed"] is False
                assert "Compilation failed:" in result["error"]
                assert "No such file or directory" in result["error"]
                assert "nonexistent.h" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_linker_error_propagation(self, mock_run):
        """Test that linker errors are properly propagated."""
        mock_run.return_value = Mock(
            returncode=1,
            stderr="/usr/bin/ld: /tmp/test.o: undefined reference to `missing_function'",
            stdout="",
        )

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(
                executor, "prepare_code", return_value="calls missing_function"
            ):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("undefined reference", test_case)

                assert result["passed"] is False
                assert "Compilation failed:" in result["error"]
                assert "undefined reference" in result["error"]
                assert "missing_function" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_type_error_propagation(self, mock_run):
        """Test that type errors are properly propagated."""
        mock_run.return_value = Mock(
            returncode=1,
            stderr="test.cpp:3:15: error: cannot convert 'const char*' to 'int'",
            stdout="",
        )

        executor = MockCompiledExecutor(["g++"], ["./"], ".cpp")

        try:
            with patch.object(executor, "prepare_code", return_value="type mismatch"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("type error", test_case)

                assert result["passed"] is False
                assert "Compilation failed:" in result["error"]
                assert "cannot convert" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_compilation_warning_handling(self, mock_run):
        """Test that compilation warnings don't prevent execution."""
        # Compilation succeeds with warnings
        compile_result = Mock(
            returncode=0, stderr="test.c:5:10: warning: unused variable 'x'", stdout=""
        )
        # Execution succeeds
        execute_result = Mock(returncode=0, stderr="", stdout="42")

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(
                executor, "prepare_code", return_value="code with warnings"
            ):
                test_case = {"parameters": {}, "expected": 42}
                result = executor.execute_test("warning code", test_case)

                # Should succeed despite warnings
                assert result["passed"] is False  # 42 != "42"
                assert result["actual"] == "42"
                assert result["expected"] == 42

                # Should have attempted both compilation and execution
                assert mock_run.call_count == 2

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_multiple_compilation_errors(self, mock_run):
        """Test handling of multiple compilation errors."""
        complex_error = """test.c: In function 'main':
test.c:5:5: error: 'undeclared_var' undeclared (first use in this function)
test.c:5:5: note: each undeclared identifier is reported only once
test.c:6:10: error: expected ';' before 'return'
test.c:7:1: error: expected declaration before '}' token"""

        mock_run.return_value = Mock(returncode=1, stderr=complex_error, stdout="")

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="multiple errors"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("multi error", test_case)

                assert result["passed"] is False
                assert "Compilation failed:" in result["error"]

                # Should contain all error information
                assert "undeclared_var" in result["error"]
                assert "expected ';'" in result["error"]
                assert "expected declaration" in result["error"]

        finally:
            executor.cleanup()


class TestRuntimeErrorPropagation:
    """Test error propagation from runtime failures."""

    @patch("subprocess.run")
    def test_segmentation_fault_propagation(self, mock_run):
        """Test that segmentation faults are properly propagated."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_result = Mock(
            returncode=139,  # Segmentation fault exit code
            stderr="Segmentation fault (core dumped)",
            stdout="",
        )

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="segfault code"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("segfault", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "Segmentation fault" in result["error"]
                assert result["actual"] is None

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_python_exception_propagation(self, mock_run):
        """Test that Python exceptions are properly propagated."""
        python_error = """Traceback (most recent call last):
  File "test.py", line 3, in <module>
    result = 10 / 0
ZeroDivisionError: division by zero"""

        mock_run.return_value = Mock(returncode=1, stderr=python_error, stdout="")

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(
                executor, "prepare_code", return_value="division by zero"
            ):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("div by zero", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "ZeroDivisionError" in result["error"]
                assert "division by zero" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_javascript_error_propagation(self, mock_run):
        """Test that JavaScript errors are properly propagated."""
        js_error = """test.js:3
console.log(undefinedVar);
            ^
ReferenceError: undefinedVar is not defined
    at Object.<anonymous> (test.js:3:13)"""

        mock_run.return_value = Mock(returncode=1, stderr=js_error, stdout="")

        executor = MockInterpretedExecutor(["node"], ".js")

        try:
            with patch.object(
                executor, "prepare_code", return_value="undefined variable"
            ):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("undefined var", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "ReferenceError" in result["error"]
                assert "undefinedVar" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_infinite_loop_timeout_propagation(self, mock_run):
        """Test that timeout errors from infinite loops are propagated."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 30)

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(
                executor, "prepare_code", return_value="while True: pass"
            ):
                test_case = {"parameters": {}, "expected": "", "timeout": 30}
                result = executor.execute_test("infinite loop", test_case)

                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                assert result["actual"] is None

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_memory_error_propagation(self, mock_run):
        """Test that memory errors are properly propagated."""
        mock_run.return_value = Mock(
            returncode=1, stderr="MemoryError: Unable to allocate array", stdout=""
        )

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(
                executor, "prepare_code", return_value="memory allocation"
            ):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("memory error", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "MemoryError" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_stack_overflow_propagation(self, mock_run):
        """Test that stack overflow errors are propagated."""
        stack_error = """Traceback (most recent call last):
  File "test.py", line 3, in recursive_func
    return recursive_func(n + 1)
  [Previous line repeated 996 more times]
RecursionError: maximum recursion depth exceeded"""

        mock_run.return_value = Mock(returncode=1, stderr=stack_error, stdout="")

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(
                executor, "prepare_code", return_value="recursive function"
            ):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("recursion", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "RecursionError" in result["error"]
                assert "maximum recursion depth" in result["error"]

        finally:
            executor.cleanup()


class TestSystemErrorPropagation:
    """Test error propagation from system-level failures."""

    @patch("subprocess.run")
    def test_file_not_found_error_propagation(self, mock_run):
        """Test that file not found errors are propagated."""
        mock_run.side_effect = FileNotFoundError(
            "No such file or directory: 'nonexistent_compiler'"
        )

        executor = MockCompiledExecutor(["nonexistent_compiler"], ["./"], ".c")

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            # File not found during compilation should be propagated
            with pytest.raises(FileNotFoundError, match="No such file or directory"):
                executor.compile(code_path)

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_permission_denied_error_propagation(self, mock_run):
        """Test that permission denied errors are propagated."""
        mock_run.side_effect = PermissionError("Permission denied")

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="test"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("permission test", test_case)

                assert result["passed"] is False
                assert "Permission denied" in result["error"]
                assert result["actual"] is None

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_disk_full_error_propagation(self, mock_run):
        """Test that disk full errors are propagated."""
        mock_run.side_effect = OSError("No space left on device")

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            with pytest.raises(OSError, match="No space left on device"):
                executor.compile(code_path)

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_command_not_executable_error(self, mock_run):
        """Test that non-executable command errors are propagated."""
        mock_run.side_effect = OSError("Exec format error")

        executor = MockInterpretedExecutor(["not_executable"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="test"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("exec error", test_case)

                assert result["passed"] is False
                assert "Exec format error" in result["error"]

        finally:
            executor.cleanup()


class TestRegistryErrorPropagation:
    """Test error propagation through the registry system."""

    def test_adapter_not_found_error(self):
        """Test error when adapter is not found in registry."""
        registry = LanguageRegistry()

        # Should return None for unsupported language
        adapter = registry.get_adapter("nonexistent_language")
        assert adapter is None

        executor = registry.get_executor("nonexistent_language")
        assert executor is None

    def test_adapter_instantiation_error(self):
        """Test error propagation when adapter instantiation fails."""

        class ErrorAdapter(LanguageAdapter):
            def __init__(self):
                raise ValueError("Adapter initialization failed")

            def get_config(self):
                pass

            def generate_prompt(self, *args, **kwargs):
                pass

            def extract_code(self, llm_response):
                pass

            def normalize_code(self, code):
                pass

        registry = LanguageRegistry()
        registry.register("error_lang", ErrorAdapter)

        # Should propagate the initialization error
        with pytest.raises(ValueError, match="Adapter initialization failed"):
            registry.get_adapter("error_lang")

    @patch("eiplgrader.languages.registry.import_module")
    def test_executor_import_error_propagation(self, mock_import):
        """Test error propagation when executor import fails."""
        mock_import.side_effect = ImportError("No module named 'missing_executor'")

        registry = LanguageRegistry()

        # Should return None when import fails
        executor = registry.get_executor("python")
        assert executor is None

    @patch("eiplgrader.languages.registry.import_module")
    def test_executor_attribute_error_propagation(self, mock_import):
        """Test error propagation when executor class is not found."""
        mock_module = Mock()
        del mock_module.PythonExecutor  # Simulate missing class
        mock_import.return_value = mock_module

        registry = LanguageRegistry()

        # Should return None when class is not found
        executor = registry.get_executor("python")
        assert executor is None

    def test_adapter_method_error_propagation(self):
        """Test error propagation from adapter method failures."""

        class FaultyAdapter(LanguageAdapter):
            def get_config(self):
                raise RuntimeError("Config generation failed")

            def generate_prompt(self, *args, **kwargs):
                raise ValueError("Prompt generation failed")

            def extract_code(self, llm_response):
                raise TypeError("Code extraction failed")

            def normalize_code(self, code):
                raise AttributeError("Code normalization failed")

        adapter = FaultyAdapter()

        # Each method should propagate its specific error
        with pytest.raises(RuntimeError, match="Config generation failed"):
            adapter.get_config()

        with pytest.raises(ValueError, match="Prompt generation failed"):
            adapter.generate_prompt("test", "func")

        with pytest.raises(TypeError, match="Code extraction failed"):
            adapter.extract_code("response")

        with pytest.raises(AttributeError, match="Code normalization failed"):
            adapter.normalize_code("code")


class TestErrorContextPreservation:
    """Test that error context and details are preserved through propagation."""

    @patch("subprocess.run")
    def test_compilation_error_context_preservation(self, mock_run):
        """Test that compilation error context is preserved."""
        detailed_error = """test.c: In function 'main':
test.c:5:12: error: 'undeclared_var' undeclared (first use in this function)
     int x = undeclared_var + 5;
            ^~~~~~~~~~~~~
test.c:5:12: note: each undeclared identifier is reported only once for each function it appears in
test.c:6:5: warning: unused variable 'x' [-Wunused-variable]
     int x = 42;
     ^~~"""

        mock_run.return_value = Mock(returncode=1, stderr=detailed_error, stdout="")

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="detailed error"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("context test", test_case)

                assert result["passed"] is False

                # Full error context should be preserved
                assert "test.c: In function 'main':" in result["error"]
                assert "undeclared_var" in result["error"]
                assert "note:" in result["error"]
                assert "warning:" in result["error"]
                assert "5:12:" in result["error"]  # Line/column info

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_runtime_error_traceback_preservation(self, mock_run):
        """Test that runtime error tracebacks are preserved."""
        detailed_traceback = """Traceback (most recent call last):
  File "test.py", line 10, in <module>
    result = calculate_value(data)
  File "test.py", line 6, in calculate_value
    return process_data(data["key"])
  File "test.py", line 3, in process_data
    return data / value
ZeroDivisionError: division by zero"""

        mock_run.return_value = Mock(returncode=1, stderr=detailed_traceback, stdout="")

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="traceback test"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("traceback", test_case)

                assert result["passed"] is False

                # Full traceback should be preserved
                assert "Traceback (most recent call last):" in result["error"]
                assert "calculate_value" in result["error"]
                assert "process_data" in result["error"]
                assert "line 10" in result["error"]
                assert "line 6" in result["error"]
                assert "line 3" in result["error"]
                assert "ZeroDivisionError: division by zero" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_error_metadata_preservation(self, mock_run):
        """Test that error metadata is preserved."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            test_case = {
                "parameters": {"x": 5, "y": 10},
                "expected": 15,
                "test_name": "addition_test",
                "description": "Tests addition of two numbers",
            }

            mock_run.return_value = Mock(
                returncode=1, stderr="Compilation failed", stdout=""
            )

            with patch.object(executor, "prepare_code", return_value="error code"):
                result = executor.execute_test("metadata test", test_case)

                # Original test case data should be preserved
                assert result["passed"] is False
                assert result["expected"] == 15
                assert result["actual"] is None

                # Error information should be added
                assert "Compilation failed" in result["error"]

        finally:
            executor.cleanup()


class TestErrorRecoveryPatterns:
    """Test error recovery and graceful degradation patterns."""

    @patch("subprocess.run")
    def test_graceful_degradation_after_error(self, mock_run):
        """Test that executors can recover after errors."""
        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            # First execution fails
            mock_run.return_value = Mock(
                returncode=1, stderr="NameError: name 'x' is not defined", stdout=""
            )

            with patch.object(executor, "prepare_code", return_value="error code"):
                test_case1 = {"parameters": {}, "expected": ""}
                result1 = executor.execute_test("error", test_case1)

                assert result1["passed"] is False
                assert "NameError" in result1["error"]

            # Second execution succeeds
            mock_run.return_value = Mock(returncode=0, stderr="", stdout="42")

            with patch.object(executor, "prepare_code", return_value="success code"):
                test_case2 = {"parameters": {}, "expected": "42"}
                result2 = executor.execute_test("success", test_case2)

                assert result2["passed"] is True
                assert result2["actual"] == "42"

            # Executor should still be functional
            assert os.path.exists(executor.temp_dir)

        finally:
            executor.cleanup()

    def test_error_isolation_between_executors(self):
        """Test that errors in one executor don't affect others."""
        executor1 = MockInterpretedExecutor(["python3"], ".py")
        executor2 = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch("subprocess.run") as mock_run:
                # Executor 1 fails
                mock_run.return_value = Mock(
                    returncode=1, stderr="RuntimeError: something went wrong", stdout=""
                )

                with patch.object(executor1, "prepare_code", return_value="error"):
                    test_case1 = {"parameters": {}, "expected": ""}
                    result1 = executor1.execute_test("error", test_case1)

                    assert result1["passed"] is False
                    assert "RuntimeError" in result1["error"]

                # Executor 2 succeeds despite executor 1's failure
                mock_run.return_value = Mock(returncode=0, stderr="", stdout="success")

                with patch.object(executor2, "prepare_code", return_value="success"):
                    test_case2 = {"parameters": {}, "expected": "success"}
                    result2 = executor2.execute_test("success", test_case2)

                    assert result2["passed"] is True
                    assert result2["actual"] == "success"

        finally:
            executor1.cleanup()
            executor2.cleanup()

    @patch("subprocess.run")
    def test_partial_success_error_handling(self, mock_run):
        """Test handling of partial success scenarios."""
        # Compilation succeeds, execution fails
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_result = Mock(returncode=1, stderr="Runtime error occurred", stdout="")

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="partial success"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("partial", test_case)

                # Should indicate runtime failure, not compilation failure
                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "Compilation failed:" not in result["error"]

                # Should have attempted both compilation and execution
                assert mock_run.call_count == 2

        finally:
            executor.cleanup()

    def test_error_aggregation_patterns(self):
        """Test patterns for aggregating multiple errors."""

        class MultiErrorExecutor(MockInterpretedExecutor):
            def execute_test(self, code, test_case):
                errors = []

                # Simulate multiple validation steps
                if not code.strip():
                    errors.append("Empty code provided")

                if not test_case.get("parameters"):
                    errors.append("No test parameters provided")

                if not test_case.get("expected"):
                    errors.append("No expected result provided")

                if errors:
                    return {
                        "passed": False,
                        "error": "Multiple validation errors: " + "; ".join(errors),
                        "actual": None,
                        "expected": test_case.get("expected"),
                    }

                return super().execute_test(code, test_case)

        executor = MultiErrorExecutor(["python3"], ".py")

        try:
            # Test case with multiple issues
            test_case = {"parameters": {}, "expected": None}
            result = executor.execute_test("", test_case)

            assert result["passed"] is False
            assert "Multiple validation errors:" in result["error"]
            assert "Empty code" in result["error"]
            assert "No test parameters" in result["error"]
            assert "No expected result" in result["error"]

        finally:
            executor.cleanup()
