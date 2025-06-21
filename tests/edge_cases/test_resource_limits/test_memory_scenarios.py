"""Tests for memory resource limit scenarios."""

import pytest
import subprocess
from unittest.mock import Mock, patch
from typing import Dict, Any

from eiplgrader.languages.executors.base_executors import (
    CompiledLanguageExecutor,
    InterpretedLanguageExecutor,
)


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


class TestMemoryExhaustionScenarios:
    """Test memory exhaustion and related resource limits."""

    @patch("subprocess.run")
    def test_out_of_memory_error_handling(self, mock_run):
        """Test handling of out-of-memory errors."""
        mock_run.return_value = Mock(
            returncode=1,
            stderr="MemoryError: Unable to allocate 8.00 GiB for an array",
            stdout="",
        )

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="memory allocation"):
                test_case = {"parameters": {"size": 1000000000}, "expected": []}
                result = executor.execute_test("large array", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "MemoryError" in result["error"]
                assert "Unable to allocate" in result["error"]
                assert result["actual"] is None

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_c_malloc_failure_handling(self, mock_run):
        """Test handling of malloc failures in C code."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_result = Mock(
            returncode=1,
            stderr="malloc: Cannot allocate memory",
            stdout="",
        )

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="malloc test"):
                test_case = {"parameters": {"size": 2147483647}, "expected": 0}
                result = executor.execute_test("malloc failure", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "Cannot allocate memory" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_stack_overflow_memory_error(self, mock_run):
        """Test stack overflow due to deep recursion."""
        stack_error = """Traceback (most recent call last):
  File "test.py", line 2, in factorial
    return n * factorial(n - 1)
  [Previous line repeated 996 more times]
RecursionError: maximum recursion depth exceeded"""

        mock_run.return_value = Mock(returncode=1, stderr=stack_error, stdout="")

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="deep recursion"):
                test_case = {"parameters": {"n": 10000}, "expected": 0}
                result = executor.execute_test("recursion", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "RecursionError" in result["error"]
                assert "maximum recursion depth" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_segmentation_fault_memory_access(self, mock_run):
        """Test segmentation faults from invalid memory access."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_result = Mock(
            returncode=139,  # SIGSEGV exit code
            stderr="Segmentation fault (core dumped)",
            stdout="",
        )

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="null pointer access"):
                test_case = {"parameters": {}, "expected": 0}
                result = executor.execute_test("segfault", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "Segmentation fault" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_javascript_memory_heap_error(self, mock_run):
        """Test JavaScript heap out of memory errors."""
        js_error = """FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory
 1: 0x10003d5f1 node::Abort() (.cold.1) [/usr/local/bin/node]
 2: 0x1000a2633 node::Abort() [/usr/local/bin/node]"""

        mock_run.return_value = Mock(returncode=134, stderr=js_error, stdout="")

        executor = MockInterpretedExecutor(["node"], ".js")

        try:
            with patch.object(executor, "prepare_code", return_value="large object"):
                test_case = {"parameters": {"size": 100000000}, "expected": {}}
                result = executor.execute_test("heap overflow", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "heap out of memory" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_java_out_of_memory_error(self, mock_run):
        """Test Java OutOfMemoryError handling."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_result = Mock(
            returncode=1,
            stderr="Exception in thread \"main\" java.lang.OutOfMemoryError: Java heap space",
            stdout="",
        )

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledExecutor(["javac", "java"], ["./"], ".java")

        try:
            with patch.object(executor, "prepare_code", return_value="large array"):
                test_case = {"parameters": {"size": 1000000000}, "expected": []}
                result = executor.execute_test("java oom", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "OutOfMemoryError" in result["error"]
                assert "Java heap space" in result["error"]

        finally:
            executor.cleanup()


class TestMemoryLeakDetection:
    """Test detection and handling of memory leaks."""

    @patch("subprocess.run")
    def test_gradual_memory_growth_detection(self, mock_run):
        """Test detection of gradual memory growth patterns."""
        # Simulate process that gradually consumes more memory
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 30)

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="memory leak"):
                test_case = {
                    "parameters": {"iterations": 1000000},
                    "expected": "complete",
                    "timeout": 30,
                }
                result = executor.execute_test("memory leak", test_case)

                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                assert result["actual"] is None

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_c_memory_leak_valgrind_simulation(self, mock_run):
        """Test simulation of memory leak detection (like valgrind would show)."""
        # Compile succeeds
        compile_result = Mock(returncode=0, stderr="", stdout="")
        # Execution succeeds but with memory issues
        execute_result = Mock(
            returncode=0,
            stderr="definitely lost: 1,024 bytes in 1 blocks",
            stdout="42",
        )

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="malloc without free"):
                test_case = {"parameters": {}, "expected": 42}
                result = executor.execute_test("memory leak", test_case)

                # Test passes but memory issues are noted
                assert result["passed"] is True  # Output matches expected
                assert result["actual"] == "42"
                assert result["expected"] == 42

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_double_free_error(self, mock_run):
        """Test double free memory corruption errors."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_result = Mock(
            returncode=134,  # SIGABRT
            stderr="free(): double free detected in tcache 2",
            stdout="",
        )

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(executor, "prepare_code", return_value="double free"):
                test_case = {"parameters": {}, "expected": 0}
                result = executor.execute_test("double free", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "double free detected" in result["error"]

        finally:
            executor.cleanup()


class TestMemoryConstrainedExecution:
    """Test execution under memory constraints."""

    @patch("subprocess.run")
    def test_memory_limited_execution_success(self, mock_run):
        """Test successful execution under memory limits."""
        # Simulate constrained but successful execution
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="42")

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="efficient code"):
                test_case = {
                    "parameters": {"data": [1, 2, 3, 4, 5]},
                    "expected": 42,
                    "memory_limit": "128MB",
                }
                result = executor.execute_test("constrained", test_case)

                assert result["passed"] is False  # "42" != 42
                assert result["actual"] == "42"
                assert result["expected"] == 42

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_memory_limit_exceeded_handling(self, mock_run):
        """Test handling when memory limits are exceeded."""
        mock_run.return_value = Mock(
            returncode=137,  # SIGKILL due to OOM
            stderr="Killed",
            stdout="",
        )

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="memory hungry"):
                test_case = {
                    "parameters": {"size": 1000000000},
                    "expected": [],
                    "memory_limit": "64MB",
                }
                result = executor.execute_test("memory limit", test_case)

                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "Killed" in result["error"]

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_swap_thrashing_detection(self, mock_run):
        """Test detection of excessive swap usage (thrashing)."""
        # Simulate slow execution due to excessive swapping
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 60)

        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="swap thrashing"):
                test_case = {
                    "parameters": {"data_size": 4000000000},  # 4GB
                    "expected": "processed",
                    "timeout": 60,
                }
                result = executor.execute_test("thrashing", test_case)

                assert result["passed"] is False
                assert result["error"] == "Execution timeout"

        finally:
            executor.cleanup()