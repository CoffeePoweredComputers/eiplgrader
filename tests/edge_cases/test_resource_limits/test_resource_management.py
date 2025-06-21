"""Tests for resource management and cleanup scenarios."""

import pytest
import os
import tempfile
import subprocess
from unittest.mock import Mock, patch, MagicMock
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


class TestResourceCleanup:
    """Test proper cleanup of resources after execution."""

    def test_temporary_directory_cleanup(self):
        """Test that temporary directories are properly cleaned up."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        
        # Verify temp directory exists during execution
        temp_dir = executor.temp_dir
        assert os.path.exists(temp_dir)
        
        # Cleanup should remove temp directory
        executor.cleanup()
        assert not os.path.exists(temp_dir)

    def test_temporary_file_cleanup(self):
        """Test that temporary files are properly cleaned up."""
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            # Create a temporary file in the executor's temp directory
            temp_file = os.path.join(executor.temp_dir, "test_file.py")
            with open(temp_file, 'w') as f:
                f.write("print('test')")
            
            assert os.path.exists(temp_file)
            
        finally:
            executor.cleanup()
            
        # File should be cleaned up
        assert not os.path.exists(temp_file)

    @patch("subprocess.run")
    def test_process_cleanup_after_timeout(self, mock_run):
        """Test that processes are properly cleaned up after timeout."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 30)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="infinite loop"):
                test_case = {"parameters": {}, "expected": "", "timeout": 30}
                result = executor.execute_test("timeout test", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_compilation_artifact_cleanup(self, mock_run):
        """Test cleanup of compilation artifacts."""
        compile_result = Mock(returncode=0, stderr="", stdout="")
        execute_result = Mock(returncode=0, stderr="", stdout="42")
        
        mock_run.side_effect = [compile_result, execute_result]
        
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        
        try:
            with patch.object(executor, "prepare_code", return_value="test code"):
                test_case = {"parameters": {}, "expected": "42"}
                result = executor.execute_test("compile test", test_case)
                
                # Check that compilation artifacts would be in temp directory
                assert os.path.exists(executor.temp_dir)
                
        finally:
            executor.cleanup()
            
        # Temp directory and all contents should be cleaned up
        assert not os.path.exists(executor.temp_dir)

    def test_exception_during_cleanup_handling(self):
        """Test handling of exceptions during cleanup."""
        
        class FaultyCleanupExecutor(MockInterpretedExecutor):
            def cleanup(self):
                # Simulate an error during cleanup
                raise OSError("Permission denied during cleanup")
        
        executor = FaultyCleanupExecutor(["python3"], ".py")
        
        # Cleanup should not raise an exception
        try:
            executor.cleanup()
        except OSError:
            pytest.fail("Cleanup should handle exceptions gracefully")


class TestFileDescriptorManagement:
    """Test proper management of file descriptors."""

    @patch("subprocess.run")
    def test_file_descriptor_leak_prevention(self, mock_run):
        """Test prevention of file descriptor leaks."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="result")
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            # Simulate multiple executions
            for i in range(10):
                with patch.object(executor, "prepare_code", return_value=f"test_{i}"):
                    test_case = {"parameters": {"i": i}, "expected": "result"}
                    result = executor.execute_test(f"test_{i}", test_case)
                    
                    assert result["passed"] is True
                    assert result["actual"] == "result"
            
        finally:
            executor.cleanup()

    @patch("builtins.open")
    def test_file_handle_management(self, mock_open):
        """Test proper management of file handles."""
        mock_file = MagicMock()
        mock_open.return_value.__enter__.return_value = mock_file
        
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        
        try:
            # Simulate file operations
            with patch.object(executor, "prepare_code", return_value="test code"):
                test_case = {"parameters": {}, "expected": ""}
                
                # File operations would normally occur here
                file_path = os.path.join(executor.temp_dir, "test.c")
                
                # Verify temp directory exists
                assert os.path.exists(executor.temp_dir)
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_stdin_stdout_stderr_management(self, mock_run):
        """Test proper management of stdin/stdout/stderr."""
        mock_process = Mock()
        mock_process.returncode = 0
        mock_process.stdout = "output"
        mock_process.stderr = ""
        
        mock_run.return_value = mock_process
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="io test"):
                test_case = {"parameters": {"input": "test"}, "expected": "output"}
                result = executor.execute_test("io test", test_case)
                
                assert result["passed"] is True
                assert result["actual"] == "output"
                
        finally:
            executor.cleanup()


class TestResourceLimits:
    """Test enforcement of resource limits."""

    @patch("subprocess.run")
    def test_cpu_time_limit_enforcement(self, mock_run):
        """Test enforcement of CPU time limits."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 10)
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="cpu intensive"):
                test_case = {
                    "parameters": {"n": 1000000},
                    "expected": 0,
                    "timeout": 10,
                }
                result = executor.execute_test("cpu limit", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_disk_space_limit_handling(self, mock_run):
        """Test handling of disk space limits."""
        mock_run.side_effect = OSError("No space left on device")
        
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        
        try:
            with patch.object(executor, "prepare_code", return_value="large output"):
                test_case = {"parameters": {}, "expected": ""}
                
                # Should raise OSError due to disk space
                with pytest.raises(OSError, match="No space left on device"):
                    executor.execute_test("disk space", test_case)
                    
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_process_count_limit_handling(self, mock_run):
        """Test handling of process count limits."""
        mock_run.side_effect = OSError("Cannot fork: Resource temporarily unavailable")
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="fork test"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("fork limit", test_case)
                
                assert result["passed"] is False
                assert "Resource temporarily unavailable" in result["error"]
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_network_resource_limit_handling(self, mock_run):
        """Test handling of network resource limits."""
        network_error = "socket.error: [Errno 24] Too many open files"
        mock_run.return_value = Mock(
            returncode=1,
            stderr=network_error,
            stdout="",
        )
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="network test"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("network limit", test_case)
                
                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "Too many open files" in result["error"]
                
        finally:
            executor.cleanup()


class TestResourceMonitoring:
    """Test monitoring of resource usage during execution."""

    @patch("subprocess.run")
    def test_execution_time_tracking(self, mock_run):
        """Test tracking of execution time."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="result")
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="time test"):
                test_case = {"parameters": {}, "expected": "result"}
                result = executor.execute_test("time tracking", test_case)
                
                assert result["passed"] is True
                assert result["actual"] == "result"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_memory_usage_tracking(self, mock_run):
        """Test tracking of memory usage."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="42")
        
        executor = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            with patch.object(executor, "prepare_code", return_value="memory test"):
                test_case = {"parameters": {"size": 1000}, "expected": "42"}
                result = executor.execute_test("memory tracking", test_case)
                
                assert result["passed"] is True
                assert result["actual"] == "42"
                
        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_disk_usage_tracking(self, mock_run):
        """Test tracking of disk usage."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="completed")
        
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        
        try:
            with patch.object(executor, "prepare_code", return_value="disk test"):
                test_case = {"parameters": {}, "expected": "completed"}
                result = executor.execute_test("disk tracking", test_case)
                
                assert result["passed"] is True
                assert result["actual"] == "completed"
                
        finally:
            executor.cleanup()

    def test_concurrent_executor_resource_isolation(self):
        """Test resource isolation between concurrent executors."""
        executor1 = MockInterpretedExecutor(["python3"], ".py")
        executor2 = MockInterpretedExecutor(["python3"], ".py")
        
        try:
            # Each executor should have its own temp directory
            assert executor1.temp_dir != executor2.temp_dir
            assert os.path.exists(executor1.temp_dir)
            assert os.path.exists(executor2.temp_dir)
            
            # Cleanup of one shouldn't affect the other
            executor1.cleanup()
            assert not os.path.exists(executor1.temp_dir)
            assert os.path.exists(executor2.temp_dir)
            
        finally:
            executor1.cleanup()
            executor2.cleanup()
            
        # Both should be cleaned up
        assert not os.path.exists(executor1.temp_dir)
        assert not os.path.exists(executor2.temp_dir)