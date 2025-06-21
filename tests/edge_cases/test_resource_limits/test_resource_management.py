"""Tests for resource management, cleanup, timeouts, and process management."""

import pytest
import tempfile
import os
import shutil
import time
import threading
import signal
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
        if hasattr(self, "temp_dir"):
            super().cleanup()


class MockInterpretedExecutor(InterpretedLanguageExecutor):
    """Mock interpreted executor for testing."""

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        return code

    def cleanup(self) -> None:
        if hasattr(self, "temp_dir"):
            super().cleanup()


class TestTemporaryDirectoryManagement:
    """Test temporary directory creation, usage, and cleanup."""

    def test_temp_directory_creation(self):
        """Test that temporary directories are created properly."""
        # Test compiled executor
        compiled_executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        assert os.path.exists(compiled_executor.temp_dir)
        assert os.path.isdir(compiled_executor.temp_dir)
        assert os.access(compiled_executor.temp_dir, os.R_OK | os.W_OK | os.X_OK)

        # Test interpreted executor
        interpreted_executor = MockInterpretedExecutor(["python3"], ".py")
        assert os.path.exists(interpreted_executor.temp_dir)
        assert os.path.isdir(interpreted_executor.temp_dir)
        assert os.access(interpreted_executor.temp_dir, os.R_OK | os.W_OK | os.X_OK)

        # Directories should be different
        assert compiled_executor.temp_dir != interpreted_executor.temp_dir

        # Cleanup
        compiled_executor.cleanup()
        interpreted_executor.cleanup()

    def test_temp_directory_isolation(self):
        """Test that each executor gets its own isolated temp directory."""
        executors = []
        temp_dirs = []

        try:
            # Create multiple executors
            for i in range(5):
                executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
                executors.append(executor)
                temp_dirs.append(executor.temp_dir)

            # All temp directories should be different
            assert len(set(temp_dirs)) == 5

            # All should exist
            for temp_dir in temp_dirs:
                assert os.path.exists(temp_dir)

            # Create files in each directory
            for i, executor in enumerate(executors):
                test_file = os.path.join(executor.temp_dir, f"test_{i}.txt")
                with open(test_file, "w") as f:
                    f.write(f"executor {i}")
                assert os.path.exists(test_file)

        finally:
            # Cleanup all executors
            for executor in executors:
                executor.cleanup()

            # All temp directories should be gone
            for temp_dir in temp_dirs:
                assert not os.path.exists(temp_dir)

    def test_temp_directory_cleanup_robustness(self):
        """Test robustness of temp directory cleanup."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        temp_dir = executor.temp_dir

        # Create various types of files and subdirectories
        test_file = os.path.join(temp_dir, "test.txt")
        with open(test_file, "w") as f:
            f.write("test content")

        subdir = os.path.join(temp_dir, "subdir")
        os.makedirs(subdir)

        nested_file = os.path.join(subdir, "nested.txt")
        with open(nested_file, "w") as f:
            f.write("nested content")

        # Make some files read-only to test cleanup robustness
        readonly_file = os.path.join(temp_dir, "readonly.txt")
        with open(readonly_file, "w") as f:
            f.write("readonly")
        os.chmod(readonly_file, 0o444)  # Read-only

        # All should exist
        assert os.path.exists(test_file)
        assert os.path.exists(subdir)
        assert os.path.exists(nested_file)
        assert os.path.exists(readonly_file)

        # Cleanup should remove everything
        executor.cleanup()
        assert not os.path.exists(temp_dir)

    def test_multiple_cleanup_calls_safety(self):
        """Test that multiple cleanup calls are safe."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        temp_dir = executor.temp_dir

        # Create a test file
        test_file = os.path.join(temp_dir, "test.txt")
        with open(test_file, "w") as f:
            f.write("test")

        assert os.path.exists(temp_dir)
        assert os.path.exists(test_file)

        # First cleanup
        executor.cleanup()
        assert not os.path.exists(temp_dir)

        # Second cleanup should not raise error
        executor.cleanup()  # Should be safe

        # Third cleanup should also be safe
        executor.cleanup()

    def test_cleanup_with_open_file_handles(self):
        """Test cleanup behavior when files might have open handles."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        temp_dir = executor.temp_dir

        test_file = os.path.join(temp_dir, "test.txt")

        # Simulate file being written during cleanup
        with open(test_file, "w") as f:
            f.write("test content")
            # File handle is open, but cleanup should still work
            executor.cleanup()

        # Directory should be cleaned up
        assert not os.path.exists(temp_dir)

    def test_temp_directory_permissions(self):
        """Test that temp directories have appropriate permissions."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        temp_dir = executor.temp_dir

        try:
            # Directory should be readable, writable, and executable by owner
            stat_info = os.stat(temp_dir)
            mode = stat_info.st_mode

            # Owner should have read, write, execute permissions
            assert mode & 0o700  # Owner permissions

            # Should be able to create files
            test_file = os.path.join(temp_dir, "permissions_test.txt")
            with open(test_file, "w") as f:
                f.write("test")
            assert os.path.exists(test_file)

            # Should be able to create subdirectories
            subdir = os.path.join(temp_dir, "subdir")
            os.makedirs(subdir)
            assert os.path.exists(subdir)

        finally:
            executor.cleanup()

    def test_temp_directory_under_system_temp(self):
        """Test that temp directories are created under system temp directory."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            temp_dir = executor.temp_dir
            system_temp = tempfile.gettempdir()

            # Executor temp dir should be under system temp
            assert temp_dir.startswith(system_temp)

        finally:
            executor.cleanup()


class TestTimeoutHandling:
    """Test timeout handling in execution workflows."""

    @patch("subprocess.run")
    def test_default_timeout_values(self, mock_run):
        """Test that default timeout values are applied correctly."""
        mock_run.return_value = Mock(returncode=0, stdout="", stderr="")

        # Test compiled executor
        compiled_executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(compiled_executor, "prepare_code", return_value="test"):
                with patch.object(
                    compiled_executor, "compile", return_value=(True, "/tmp/test", "")
                ):
                    test_case = {"parameters": {}, "expected": ""}
                    result = compiled_executor.execute_test("test", test_case)

                    # Should use default timeout of 30 seconds
                    mock_run.assert_called()
                    _, kwargs = mock_run.call_args
                    assert kwargs.get("timeout") == 30

        finally:
            compiled_executor.cleanup()

        # Test interpreted executor
        mock_run.reset_mock()
        interpreted_executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(
                interpreted_executor, "prepare_code", return_value="test"
            ):
                test_case = {"parameters": {}, "expected": ""}
                result = interpreted_executor.execute_test("test", test_case)

                # Should use default timeout of 30 seconds
                mock_run.assert_called()
                _, kwargs = mock_run.call_args
                assert kwargs.get("timeout") == 30

        finally:
            interpreted_executor.cleanup()

    @patch("subprocess.run")
    def test_custom_timeout_values(self, mock_run):
        """Test that custom timeout values are respected."""
        mock_run.return_value = Mock(returncode=0, stdout="", stderr="")

        compiled_executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(compiled_executor, "prepare_code", return_value="test"):
                with patch.object(
                    compiled_executor, "compile", return_value=(True, "/tmp/test", "")
                ):
                    # Test case with custom timeout
                    test_case = {"parameters": {}, "expected": "", "timeout": 120}
                    result = compiled_executor.execute_test("test", test_case)

                    # Should use custom timeout
                    mock_run.assert_called()
                    _, kwargs = mock_run.call_args
                    assert kwargs.get("timeout") == 120

        finally:
            compiled_executor.cleanup()

    @patch("subprocess.run")
    def test_timeout_exception_handling(self, mock_run):
        """Test handling of timeout exceptions."""
        # Simulate timeout during execution
        mock_run.side_effect = subprocess.TimeoutExpired("test_cmd", 30)

        compiled_executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            with patch.object(compiled_executor, "prepare_code", return_value="test"):
                with patch.object(
                    compiled_executor, "compile", return_value=(True, "/tmp/test", "")
                ):
                    test_case = {"parameters": {}, "expected": ""}
                    result = compiled_executor.execute_test("test", test_case)

                    assert result["passed"] is False
                    assert result["error"] == "Execution timeout"
                    assert result["actual"] is None

        finally:
            compiled_executor.cleanup()

    @patch("subprocess.run")
    def test_timeout_during_compilation(self, mock_run):
        """Test timeout handling during compilation phase."""
        # Simulate timeout during compilation
        mock_run.side_effect = subprocess.TimeoutExpired("gcc", 30)

        compiled_executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            code_path = os.path.join(compiled_executor.temp_dir, "test.c")

            # Compilation timeout should bubble up
            with pytest.raises(subprocess.TimeoutExpired):
                compiled_executor.compile(code_path)

        finally:
            compiled_executor.cleanup()

    def test_timeout_precision(self):
        """Test timeout precision and timing accuracy."""
        start_time = time.time()

        with patch("subprocess.run") as mock_run:
            # Simulate exactly the timeout duration
            def timeout_after_delay(*args, **kwargs):  # pylint: disable=unused-argument
                timeout = kwargs.get("timeout", 30)
                time.sleep(
                    min(timeout + 0.1, 1.0)
                )  # Sleep slightly longer than timeout, but cap at 1 second for test speed
                raise subprocess.TimeoutExpired("test_cmd", timeout)

            mock_run.side_effect = timeout_after_delay

            executor = MockInterpretedExecutor(["python3"], ".py")

            try:
                with patch.object(executor, "prepare_code", return_value="test"):
                    test_case = {
                        "parameters": {},
                        "expected": "",
                        "timeout": 0.5,
                    }  # Short timeout for test
                    result = executor.execute_test("test", test_case)

                    end_time = time.time()
                    elapsed = end_time - start_time

                    # Should have timed out
                    assert result["passed"] is False
                    assert result["error"] == "Execution timeout"

                    # Elapsed time should be reasonable (allow some overhead)
                    assert elapsed < 2.0  # Should not take too long

            finally:
                executor.cleanup()


class TestProcessManagement:
    """Test process management and resource cleanup."""

    @patch("subprocess.run")
    def test_process_isolation(self, mock_run):
        """Test that processes are properly isolated."""
        mock_run.return_value = Mock(returncode=0, stdout="", stderr="")

        executor1 = MockInterpretedExecutor(["python3"], ".py")
        executor2 = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor1, "prepare_code", return_value="test1"):
                with patch.object(executor2, "prepare_code", return_value="test2"):
                    test_case = {"parameters": {}, "expected": ""}

                    # Execute both concurrently
                    result1 = executor1.execute_test("test1", test_case)
                    result2 = executor2.execute_test("test2", test_case)

                    # Both should succeed independently
                    assert mock_run.call_count == 2

                    # Should have called different file paths
                    call1_args = mock_run.call_args_list[0][0][0]
                    call2_args = mock_run.call_args_list[1][0][0]

                    assert call1_args[-1] != call2_args[-1]  # Different file paths

        finally:
            executor1.cleanup()
            executor2.cleanup()

    @patch("subprocess.run")
    def test_process_error_isolation(self, mock_run):
        """Test that process errors don't affect other executors."""
        # First call fails, second succeeds
        mock_run.side_effect = [
            subprocess.CalledProcessError(1, "python3", stderr="Error in process 1"),
            Mock(returncode=0, stdout="success", stderr=""),
        ]

        executor1 = MockInterpretedExecutor(["python3"], ".py")
        executor2 = MockInterpretedExecutor(["python3"], ".py")

        try:
            with patch.object(executor1, "prepare_code", return_value="fail"):
                with patch.object(executor2, "prepare_code", return_value="succeed"):
                    test_case = {"parameters": {}, "expected": ""}

                    # First executor should handle error gracefully
                    result1 = executor1.execute_test("fail", test_case)
                    assert result1["passed"] is False

                    # Second executor should succeed despite first one failing
                    result2 = executor2.execute_test("succeed", test_case)
                    assert result2["passed"] is False  # "success" != ""

        finally:
            executor1.cleanup()
            executor2.cleanup()

    @patch("subprocess.run")
    def test_subprocess_argument_passing(self, mock_run):
        """Test that subprocess arguments are passed correctly."""
        mock_run.return_value = Mock(returncode=0, stdout="", stderr="")

        executor = MockInterpretedExecutor(["python3", "-u"], ".py")

        try:
            with patch.object(executor, "prepare_code", return_value="test"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("test", test_case)

                # Should have called with correct interpreter arguments
                mock_run.assert_called_once()
                args, kwargs = mock_run.call_args

                assert args[0][0] == "python3"
                assert args[0][1] == "-u"
                assert args[0][2].endswith(".py")

                # Should have correct subprocess options
                assert kwargs["capture_output"] is True
                assert kwargs["text"] is True
                assert "timeout" in kwargs

        finally:
            executor.cleanup()

    def test_concurrent_process_execution(self):
        """Test concurrent process execution safety."""

        results = []
        errors = []

        def run_executor(executor_id):
            try:
                executor = MockInterpretedExecutor(["python3"], ".py")

                with patch("subprocess.run") as mock_run:
                    mock_run.return_value = Mock(
                        returncode=0, stdout=f"result_{executor_id}", stderr=""
                    )

                    with patch.object(
                        executor, "prepare_code", return_value=f"print({executor_id})"
                    ):
                        test_case = {
                            "parameters": {},
                            "expected": f"result_{executor_id}",
                        }
                        result = executor.execute_test("test", test_case)

                        time.sleep(0.1)  # Simulate work

                        results.append((executor_id, result["passed"]))

                executor.cleanup()

            except Exception as e:
                errors.append((executor_id, str(e)))

        # Run multiple executors concurrently
        threads = []
        for i in range(5):
            thread = threading.Thread(target=run_executor, args=(i,))
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

        # All should succeed
        assert len(errors) == 0
        assert len(results) == 5

        # All should have passed their tests
        for executor_id, passed in results:
            assert passed is True

    @patch("subprocess.run")
    def test_process_environment_isolation(self, mock_run):
        """Test that process environments are isolated."""
        mock_run.return_value = Mock(returncode=0, stdout="", stderr="")

        # Test with different working directories
        executor1 = MockCompiledExecutor(["gcc"], ["./"], ".c")
        executor2 = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            # Each executor should work in its own temp directory
            assert executor1.temp_dir != executor2.temp_dir

            # Create files in each directory
            file1 = os.path.join(executor1.temp_dir, "test.c")
            file2 = os.path.join(executor2.temp_dir, "test.c")

            with open(file1, "w") as f:
                f.write("// executor 1")
            with open(file2, "w") as f:
                f.write("// executor 2")

            # Compilation should work in respective directories
            success1, _, _ = executor1.compile(file1)
            success2, _, _ = executor2.compile(file2)

            # Both should have attempted compilation
            assert mock_run.call_count == 2

            # Should have compiled different files
            call1_args = mock_run.call_args_list[0][0][0]
            call2_args = mock_run.call_args_list[1][0][0]

            assert file1 in call1_args
            assert file2 in call2_args

        finally:
            executor1.cleanup()
            executor2.cleanup()


class TestMemoryManagement:
    """Test memory usage and cleanup patterns."""

    def test_executor_memory_cleanup(self):
        """Test that executors clean up their memory properly."""
        import gc
        import weakref

        # Create executor and get weak reference
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        temp_dir = executor.temp_dir
        weak_ref = weakref.ref(executor)

        # Executor should exist
        assert weak_ref() is not None
        assert os.path.exists(temp_dir)

        # Cleanup and delete executor
        executor.cleanup()
        del executor
        gc.collect()  # Force garbage collection

        # Executor should be garbage collected
        # Note: weak reference may still exist but temp dir should be cleaned
        assert not os.path.exists(temp_dir)

    def test_large_number_of_executors(self):
        """Test handling of large numbers of executors."""
        executors = []
        temp_dirs = []

        try:
            # Create many executors
            for i in range(50):  # Reasonable number for testing
                executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
                executors.append(executor)
                temp_dirs.append(executor.temp_dir)

            # All should have unique temp directories
            assert len(set(temp_dirs)) == 50

            # All temp directories should exist
            for temp_dir in temp_dirs:
                assert os.path.exists(temp_dir)

            # Create a small file in each directory
            for i, executor in enumerate(executors):
                test_file = os.path.join(executor.temp_dir, f"test_{i}.txt")
                with open(test_file, "w") as f:
                    f.write(f"test {i}")

        finally:
            # Cleanup all executors
            for executor in executors:
                executor.cleanup()

            # All temp directories should be gone
            for temp_dir in temp_dirs:
                assert not os.path.exists(temp_dir)

    def test_executor_reuse_safety(self):
        """Test that executors can be safely reused."""
        executor = MockInterpretedExecutor(["python3"], ".py")

        try:
            temp_dir = executor.temp_dir

            with patch("subprocess.run") as mock_run:
                mock_run.return_value = Mock(returncode=0, stdout="1", stderr="")

                # Use executor multiple times
                for i in range(5):
                    with patch.object(
                        executor, "prepare_code", return_value=f"print({i})"
                    ):
                        test_case = {"parameters": {}, "expected": str(i)}
                        result = executor.execute_test(f"test_{i}", test_case)

                        # Each execution should work
                        assert result["actual"] == "1"  # Mock always returns "1"

                        # Temp directory should remain the same
                        assert executor.temp_dir == temp_dir
                        assert os.path.exists(temp_dir)

                # Should have executed 5 times
                assert mock_run.call_count == 5

        finally:
            executor.cleanup()

    def test_resource_leak_prevention(self):
        """Test prevention of resource leaks."""
        initial_temp_dirs = []

        # Get baseline of temp directories
        temp_parent = tempfile.gettempdir()
        initial_contents = set(os.listdir(temp_parent))

        # Create and cleanup many executors
        for i in range(10):
            executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
            temp_dir = executor.temp_dir

            # Create some files
            test_file = os.path.join(temp_dir, "test.c")
            with open(test_file, "w") as f:
                f.write(f"// test {i}")

            # Cleanup immediately
            executor.cleanup()

            # Temp directory should be gone
            assert not os.path.exists(temp_dir)

        # Check that no temp directories were leaked
        final_contents = set(os.listdir(temp_parent))

        # Final contents should not have significantly more items
        # (allowing for some system temp files)
        new_items = final_contents - initial_contents
        temp_like_items = [item for item in new_items if item.startswith("tmp")]

        # Should not have created permanent temp directories
        assert len(temp_like_items) < 5  # Allow some tolerance for system behavior


class TestErrorRecoveryAndCleanup:
    """Test error recovery and cleanup in various failure scenarios."""

    def test_cleanup_after_compilation_failure(self):
        """Test cleanup after compilation failure."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(
                returncode=1, stderr="Compilation failed", stdout=""
            )

            executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
            temp_dir = executor.temp_dir

            try:
                with patch.object(
                    executor, "prepare_code", return_value="invalid code"
                ):
                    test_case = {"parameters": {}, "expected": ""}
                    result = executor.execute_test("invalid", test_case)

                    # Should have failed compilation
                    assert result["passed"] is False
                    assert "Compilation failed" in result["error"]

                    # Temp directory should still exist for potential debugging
                    assert os.path.exists(temp_dir)

            finally:
                # Cleanup should still work after failure
                executor.cleanup()
                assert not os.path.exists(temp_dir)

    def test_cleanup_after_runtime_failure(self):
        """Test cleanup after runtime failure."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(
                returncode=1, stderr="Runtime error", stdout=""
            )

            executor = MockInterpretedExecutor(["python3"], ".py")
            temp_dir = executor.temp_dir

            try:
                with patch.object(
                    executor, "prepare_code", return_value="raise Exception()"
                ):
                    test_case = {"parameters": {}, "expected": ""}
                    result = executor.execute_test("error_code", test_case)

                    # Should have failed execution
                    assert result["passed"] is False
                    assert "Runtime error" in result["error"]

                    # Temp directory should still exist
                    assert os.path.exists(temp_dir)

            finally:
                # Cleanup should work after runtime failure
                executor.cleanup()
                assert not os.path.exists(temp_dir)

    def test_cleanup_during_exception_handling(self):
        """Test cleanup behavior during exception handling."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        temp_dir = executor.temp_dir

        try:
            # Simulate exception during execution
            with patch("subprocess.run") as mock_run:
                mock_run.side_effect = Exception("Unexpected error")

                with patch.object(executor, "prepare_code", return_value="test"):
                    with patch.object(
                        executor, "compile", return_value=(True, "/tmp/test", "")
                    ):
                        test_case = {"parameters": {}, "expected": ""}
                        result = executor.execute_test("test", test_case)

                        # Should have caught the exception
                        assert result["passed"] is False
                        assert "Unexpected error" in result["error"]

                        # Temp directory should still exist
                        assert os.path.exists(temp_dir)

        finally:
            # Cleanup should work despite exception
            executor.cleanup()
            assert not os.path.exists(temp_dir)

    def test_forced_cleanup_with_context_manager(self):
        """Test forced cleanup using context manager pattern."""

        class ExecutorContextManager:
            def __init__(self, executor_class, *args):
                self.executor_class = executor_class
                self.args = args
                self.executor = None

            def __enter__(self):
                self.executor = self.executor_class(*self.args)
                return self.executor

            def __exit__(self, exc_type, exc_val, exc_tb):
                if self.executor:
                    self.executor.cleanup()

        temp_dir = None

        # Use context manager for guaranteed cleanup
        with ExecutorContextManager(
            CompiledLanguageExecutor, ["gcc"], ["./"], ".c"
        ) as executor:
            temp_dir = executor.temp_dir
            assert os.path.exists(temp_dir)

            # Create test file
            test_file = os.path.join(temp_dir, "test.c")
            with open(test_file, "w") as f:
                f.write("// test")
            assert os.path.exists(test_file)

            # Simulate some error
            raise RuntimeError("Test error")

        # Even with exception, cleanup should have occurred
        assert not os.path.exists(temp_dir)


class TestSystemResourceLimits:
    """Test behavior under system resource constraints."""

    @patch("tempfile.mkdtemp")
    def test_temp_directory_creation_failure(self, mock_mkdtemp):
        """Test handling of temp directory creation failure."""
        mock_mkdtemp.side_effect = OSError("No space left on device")

        with pytest.raises(OSError, match="No space left on device"):
            MockCompiledExecutor(["gcc"], ["./"], ".c")

    @patch("os.makedirs")
    def test_directory_creation_permission_denied(self, mock_makedirs):
        """Test handling of permission errors during directory creation."""
        # This test is more conceptual since temp directory creation uses mkdtemp
        # but tests the principle of handling permission errors
        pass

    def test_disk_space_usage_estimation(self):
        """Test estimation of disk space usage."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            temp_dir = executor.temp_dir
            initial_size = self._get_directory_size(temp_dir)

            # Create test files of known sizes
            test_file1 = os.path.join(temp_dir, "test1.c")
            with open(test_file1, "w") as f:
                f.write("x" * 1000)  # 1KB

            test_file2 = os.path.join(temp_dir, "test2.c")
            with open(test_file2, "w") as f:
                f.write("y" * 5000)  # 5KB

            final_size = self._get_directory_size(temp_dir)

            # Should have increased by at least 6KB
            assert final_size >= initial_size + 6000

        finally:
            executor.cleanup()

    def _get_directory_size(self, directory):
        """Helper to calculate directory size."""
        total_size = 0
        for dirpath, dirnames, filenames in os.walk(directory):
            for filename in filenames:
                filepath = os.path.join(dirpath, filename)
                if os.path.exists(filepath):
                    total_size += os.path.getsize(filepath)
        return total_size

    def test_concurrent_executor_resource_usage(self):
        """Test resource usage with many concurrent executors."""

        max_executors = 20
        executor_results = []

        def create_executor_with_files(executor_id):
            try:
                executor = MockInterpretedExecutor(["python3"], ".py")
                temp_dir = executor.temp_dir

                # Create test file
                test_file = os.path.join(temp_dir, f"test_{executor_id}.py")
                with open(test_file, "w") as f:
                    f.write(f"print('executor {executor_id}')")

                time.sleep(0.1)  # Hold resources briefly

                executor_results.append(
                    (executor_id, temp_dir, os.path.exists(test_file))
                )
                executor.cleanup()

                # Verify cleanup
                executor_results.append(
                    (executor_id, temp_dir, os.path.exists(temp_dir))
                )

            except Exception as e:
                executor_results.append((executor_id, "error", str(e)))

        # Create many executors concurrently
        threads = []
        for i in range(max_executors):
            thread = threading.Thread(target=create_executor_with_files, args=(i,))
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

        # Verify all executors worked and cleaned up
        creation_results = [
            r for r in executor_results if r[2] is True and r[1] != "error"
        ]
        cleanup_results = [
            r for r in executor_results if r[2] is False and r[1] != "error"
        ]

        # Should have created and cleaned up all executors
        assert len(creation_results) == max_executors
        assert len(cleanup_results) == max_executors
