"""Tests for timeout scenarios and execution time limits."""

import pytest
import subprocess
import time
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


class TestBasicTimeoutScenarios:
    """Test basic timeout scenarios for different types of code."""

    @patch("subprocess.run")
    def test_infinite_loop_timeout(self, mock_run):
        """Test timeout handling for infinite loops."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 5)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="while True: pass"):
                test_case = {
                    "parameters": {},
                    "expected": "never_reached",
                    "timeout": 5,
                }
                result = executor.execute_test("infinite loop", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                assert result["actual"] is None
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_long_computation_timeout(self, mock_run):
        """Test timeout for legitimately long computations."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 10)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="heavy computation"):
                test_case = {
                    "parameters": {"n": 1000000000},
                    "expected": 42,
                    "timeout": 10,
                }
                result = executor.execute_test("long computation", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                assert result["actual"] is None
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_recursive_timeout(self, mock_run):
        """Test timeout for infinite recursion."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 3)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="infinite recursion"):
                test_case = {
                    "parameters": {"depth": -1},
                    "expected": 0,
                    "timeout": 3,
                }
                result = executor.execute_test("infinite recursion", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                assert result["actual"] is None
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_io_blocking_timeout(self, mock_run):
        """Test timeout for I/O blocking operations."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 8)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="blocking I/O"):
                test_case = {
                    "parameters": {"file": "/dev/stdin"},
                    "expected": "input",
                    "timeout": 8,
                }
                result = executor.execute_test("blocking io", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                assert result["actual"] is None
                
        finally:
            executor.cleanup()


class TestCompilationTimeouts:
    """Test timeout scenarios during compilation phase."""

    @patch("subprocess.run")
    def test_compilation_timeout(self, mock_run):
        """Test timeout during compilation phase."""
        mock_run.side_effect = subprocess.TimeoutExpired("gcc", 30)
        
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        
        try:
            with patch.object(executor, "prepare_code", return_value="complex template"):
                test_case = {"parameters": {}, "expected": 0}
                
                # Should raise TimeoutExpired during compilation
                with pytest.raises(subprocess.TimeoutExpired):
                    executor.execute_test("compilation timeout", test_case)
                    
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_linking_timeout(self, mock_run):
        """Test timeout during linking phase."""
        # Compilation succeeds
        compile_result = Mock(returncode=0, stderr="", stdout="")
        # Linking times out
        link_timeout = subprocess.TimeoutExpired("ld", 15)
        
        mock_run.side_effect = [compile_result, link_timeout]
        
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        
        try:
            with patch.object(executor, "prepare_code", return_value="complex linking"):
                test_case = {"parameters": {}, "expected": 0}
                
                # Should raise TimeoutExpired during linking
                with pytest.raises(subprocess.TimeoutExpired):
                    executor.execute_test("linking timeout", test_case)
                    
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_template_instantiation_timeout(self, mock_run):
        """Test timeout during C++ template instantiation."""
        template_timeout = subprocess.TimeoutExpired("g++", 45)
        mock_run.side_effect = template_timeout
        
        executor = MockCompiledExecutor(["g++"], ["./"], ".cpp")
        
        try:
            with patch.object(executor, "prepare_code", return_value="complex templates"):
                test_case = {"parameters": {}, "expected": 0}
                
                with pytest.raises(subprocess.TimeoutExpired):
                    executor.execute_test("template timeout", test_case)
                    
        finally:
            executor.cleanup()


class TestLanguageSpecificTimeouts:
    """Test timeout scenarios specific to different languages."""

    @patch("subprocess.run")
    def test_python_gil_timeout(self, mock_run):
        """Test timeout with Python GIL-related blocking."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 12)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="threading issue"):
                test_case = {
                    "parameters": {"threads": 10},
                    "expected": "synchronized",
                    "timeout": 12,
                }
                result = executor.execute_test("gil timeout", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_javascript_event_loop_timeout(self, mock_run):
        """Test timeout with JavaScript event loop blocking."""
        mock_run.side_effect = subprocess.TimeoutExpired("node", 6)
        
        executor = MockInterpretedExecutor(["node"], ".js")
        
        try:
            with patch.object(executor, "prepare_code", return_value="event loop block"):
                test_case = {
                    "parameters": {"delay": 10000},
                    "expected": "resolved",
                    "timeout": 6,
                }
                result = executor.execute_test("event loop timeout", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_java_garbage_collection_timeout(self, mock_run):
        """Test timeout due to excessive garbage collection."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_timeout = subprocess.TimeoutExpired("java", 20)
        
        mock_run.side_effect = [compile_result, execute_timeout]
        
        executor = MockCompiledExecutor(["javac", "java"], ["./"], ".java")
        
        try:
            with patch.object(executor, "prepare_code", return_value="gc pressure"):
                test_case = {
                    "parameters": {"objects": 1000000},
                    "expected": "complete",
                    "timeout": 20,
                }
                result = executor.execute_test("gc timeout", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_haskell_lazy_evaluation_timeout(self, mock_run):
        """Test timeout with Haskell lazy evaluation issues."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_timeout = subprocess.TimeoutExpired("./main", 15)
        
        mock_run.side_effect = [compile_result, execute_timeout]
        
        executor = MockCompiledExecutor(["ghc"], ["./"], ".hs")
        
        try:
            with patch.object(executor, "prepare_code", return_value="lazy evaluation"):
                test_case = {
                    "parameters": {"list": "infinite"},
                    "expected": [1, 2, 3],
                    "timeout": 15,
                }
                result = executor.execute_test("lazy timeout", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_go_goroutine_leak_timeout(self, mock_run):
        """Test timeout due to goroutine leaks."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_timeout = subprocess.TimeoutExpired("./main", 10)
        
        mock_run.side_effect = [compile_result, execute_timeout]
        
        executor = MockCompiledExecutor(["go"], ["./"], ".go")
        
        try:
            with patch.object(executor, "prepare_code", return_value="goroutine leak"):
                test_case = {
                    "parameters": {"goroutines": 10000},
                    "expected": "done",
                    "timeout": 10,
                }
                result = executor.execute_test("goroutine timeout", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()


class TestTimeoutConfigurationHandling:
    """Test different timeout configuration scenarios."""

    @patch("subprocess.run")
    def test_default_timeout_handling(self, mock_run):
        """Test handling when no timeout is specified."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="42")
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="quick execution"):
                test_case = {"parameters": {}, "expected": "42"}
                result = executor.execute_test("no timeout", test_case)
                
                assert result["passed"] is True
                assert result["actual"] == "42"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_very_short_timeout(self, mock_run):
        """Test handling of very short timeouts."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 0.1)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="any code"):
                test_case = {
                    "parameters": {},
                    "expected": "result",
                    "timeout": 0.1,
                }
                result = executor.execute_test("short timeout", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_very_long_timeout(self, mock_run):
        """Test handling of very long timeouts."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="completed")
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="normal execution"):
                test_case = {
                    "parameters": {},
                    "expected": "completed",
                    "timeout": 3600,  # 1 hour
                }
                result = executor.execute_test("long timeout", test_case)
                
                assert result["passed"] is True
                assert result["actual"] == "completed"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_fractional_timeout(self, mock_run):
        """Test handling of fractional timeouts."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 2.5)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="slow code"):
                test_case = {
                    "parameters": {},
                    "expected": "result",
                    "timeout": 2.5,
                }
                result = executor.execute_test("fractional timeout", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()


class TestTimeoutRecovery:
    """Test recovery and cleanup after timeout scenarios."""

    @patch("subprocess.run")
    def test_executor_reuse_after_timeout(self, mock_run):
        """Test that executors can be reused after timeout."""
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            # First execution times out
            mock_run.side_effect = subprocess.TimeoutExpired("python3", 5)
            
            with patch.object(executor, "prepare_code", return_value="timeout code"):
                test_case1 = {"parameters": {}, "expected": "", "timeout": 5}
                result1 = executor.execute_test("timeout", test_case1)
                
                assert result1["passed"] is False
                assert result1["error"] == "Execution timeout"
            
            # Second execution succeeds
            mock_run.side_effect = None
            mock_run.return_value = Mock(returncode=0, stderr="", stdout="success")
            
            with patch.object(executor, "prepare_code", return_value="success code"):
                test_case2 = {"parameters": {}, "expected": "success"}
                result2 = executor.execute_test("success", test_case2)
                
                assert result2["passed"] is True
                assert result2["actual"] == "success"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_timeout_during_cleanup(self, mock_run):
        """Test handling of timeout during cleanup operations."""
        
        class TimeoutCleanupExecutor(MockInterpretedExecutor):
            def cleanup(self):
                # Simulate cleanup operation that might timeout
                try:
                    super().cleanup()
                except Exception:
                    # Cleanup should handle exceptions gracefully
                    pass
        
        executor = TimeoutCleanupExecutor(["python3"], ".py")
        
        try:
            mock_run.return_value = Mock(returncode=0, stderr="", stdout="42")
            
            with patch.object(executor, "prepare_code", return_value="test"):
                test_case = {"parameters": {}, "expected": "42"}
                result = executor.execute_test("cleanup test", test_case)
                
                assert result["passed"] is True
                
        finally:
            # This should not raise an exception
            executor.cleanup()

    @patch("subprocess.run")
    def test_multiple_timeouts_handling(self, mock_run):
        """Test handling of multiple consecutive timeouts."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 2)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            # Multiple timeout scenarios
            for i in range(3):
                with patch.object(executor, "prepare_code", return_value=f"timeout_{i}"):
                    test_case = {
                        "parameters": {"iteration": i},
                        "expected": f"result_{i}",
                        "timeout": 2,
                    }
                    result = executor.execute_test(f"timeout_{i}", test_case)
                    
                    assert result["passed"] is False
                    assert result["error"] == "Execution timeout"
                    assert result["actual"] is None
                    
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_timeout_error_preservation(self, mock_run):
        """Test that timeout errors preserve execution context."""
        timeout_exception = subprocess.TimeoutExpired("python3", 7)
        timeout_exception.stdout = b"partial output"
        timeout_exception.stderr = b"warning: slow execution"
        
        mock_run.side_effect = timeout_exception
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="slow execution"):
                test_case = {
                    "parameters": {"delay": 10},
                    "expected": "complete",
                    "timeout": 7,
                }
                result = executor.execute_test("context timeout", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                assert result["actual"] is None
                assert result["expected"] == "complete"
                
        finally:
            executor.cleanup()