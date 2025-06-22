"""Tests for compilation workflows across compiled languages."""

import pytest
import tempfile
import os
import shutil
import json
import subprocess
from unittest.mock import Mock, patch, MagicMock, call
from typing import Dict, Any

from eiplgrader.languages.executors.base_executors import CompiledLanguageExecutor
from eiplgrader.languages.executors.c_executor import CExecutor
from eiplgrader.languages.executors.cpp_executor import CppExecutor
from eiplgrader.languages.executors.java_executor import JavaExecutor
from eiplgrader.languages.executors.go_executor import GoExecutor
from eiplgrader.languages.executors.haskell_executor import HaskellExecutor


class MockCompiledLanguageExecutor(CompiledLanguageExecutor):
    """Concrete test implementation of CompiledLanguageExecutor."""

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Simple prepare_code implementation for testing."""
        return code


class TestCompilationWorkflowBase:
    """Test base compilation workflow functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()

    def teardown_method(self):
        """Clean up test fixtures."""
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    @patch("subprocess.run")
    def test_compilation_success_workflow(self, mock_run):
        """Test successful compilation workflow."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="")

        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc"], run_cmd=["./"], file_ext=".c", use_json_input=False
        )

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            # Test compilation
            success, output_path, error = executor.compile(code_path)

            assert success is True
            assert output_path == code_path.replace(".c", "")
            assert error == ""

            # Verify correct command was called
            expected_cmd = ["gcc", "-o", output_path, code_path]
            mock_run.assert_called_once_with(
                expected_cmd, capture_output=True, text=True
            )

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_compilation_failure_workflow(self, mock_run):
        """Test compilation failure workflow."""
        mock_run.return_value = Mock(
            returncode=1, stderr="error: 'undeclared_var' undeclared", stdout=""
        )

        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc"], run_cmd=["./"], file_ext=".c", use_json_input=False
        )

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            # Test compilation failure
            success, output_path, error = executor.compile(code_path)

            assert success is False
            assert output_path == code_path.replace(".c", "")
            assert "undeclared_var" in error

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_compilation_with_warnings_workflow(self, mock_run):
        """Test compilation with warnings (should still succeed)."""
        mock_run.return_value = Mock(
            returncode=0, stderr="warning: unused variable 'x'", stdout=""
        )

        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc", "-Wall"],
            run_cmd=["./"],
            file_ext=".c",
            use_json_input=False,
        )

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            # Test compilation with warnings
            success, output_path, error = executor.compile(code_path)

            assert success is True  # Should succeed despite warnings
            assert "warning" in error

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_compilation_timeout_handling(self, mock_run):
        """Test compilation timeout handling."""
        mock_run.side_effect = subprocess.TimeoutExpired("gcc", 30)

        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc"], run_cmd=["./"], file_ext=".c", use_json_input=False
        )

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            # Test compilation timeout
            with pytest.raises(subprocess.TimeoutExpired):
                executor.compile(code_path)

        finally:
            executor.cleanup()


class TestCCompilationWorkflow:
    """Test C-specific compilation workflow."""

    def test_c_executor_initialization(self):
        """Test C executor initialization."""
        try:
            executor = CExecutor()

            assert executor.compile_cmd == ["gcc"]
            assert executor.run_cmd is None
            assert executor.file_ext == ".c"
            assert executor.use_json_input is False
            assert hasattr(executor, "temp_dir")
            assert os.path.exists(executor.temp_dir)

        except ImportError:
            pytest.skip("CExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_c_compilation_command(self, mock_run):
        """Test C compilation command structure."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="")

        try:
            executor = CExecutor()

            code_path = os.path.join(executor.temp_dir, "test.c")
            success, output_path, error = executor.compile(code_path)

            # Verify GCC was called with correct arguments
            expected_output = code_path.replace(".c", "")
            expected_cmd = ["gcc", "-o", expected_output, code_path]
            mock_run.assert_called_once_with(
                expected_cmd, capture_output=True, text=True
            )

        except ImportError:
            pytest.skip("CExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_c_compilation_error_scenarios(self, mock_run):
        """Test various C compilation error scenarios."""
        try:
            executor = CExecutor()
            code_path = os.path.join(executor.temp_dir, "test.c")

            # Test syntax error
            mock_run.return_value = Mock(
                returncode=1,
                stderr="test.c:1:1: error: expected declaration specifiers",
                stdout="",
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "expected declaration specifiers" in error

            # Test missing header
            mock_run.return_value = Mock(
                returncode=1,
                stderr="test.c:1:10: fatal error: missing.h: No such file",
                stdout="",
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "No such file" in error

        except ImportError:
            pytest.skip("CExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()


class TestCppCompilationWorkflow:
    """Test C++ specific compilation workflow."""

    def test_cpp_executor_initialization(self):
        """Test C++ executor initialization."""
        try:
            executor = CppExecutor()

            assert executor.compile_cmd == ["g++", "-std=c++17"]
            assert executor.run_cmd == ["./a.out"]
            assert executor.file_ext == ".cpp"
            assert executor.use_json_input is False

        except ImportError:
            pytest.skip("CppExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_cpp_compilation_command(self, mock_run):
        """Test C++ compilation command structure."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="")

        try:
            executor = CppExecutor()

            code_path = os.path.join(executor.temp_dir, "test.cpp")
            success, output_path, error = executor.compile(code_path)

            # Verify g++ was called
            expected_output = code_path.replace(".cpp", "")
            expected_cmd = ["g++", "-std=c++17", "-o", expected_output, code_path]
            mock_run.assert_called_once_with(
                expected_cmd, capture_output=True, text=True
            )

        except ImportError:
            pytest.skip("CppExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()


class TestJavaCompilationWorkflow:
    """Test Java specific compilation workflow."""

    def test_java_executor_initialization(self):
        """Test Java executor initialization."""
        try:
            executor = JavaExecutor()

            assert executor.compile_cmd == ["javac"]
            assert executor.run_cmd == ["java"]
            assert executor.file_ext == ".java"
            assert executor.use_json_input is False

        except ImportError:
            pytest.skip("JavaExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_java_compilation_command(self, mock_run):
        """Test Java compilation command structure."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="")

        try:
            executor = JavaExecutor()

            code_path = os.path.join(executor.temp_dir, "Test.java")
            success, output_path, error = executor.compile(code_path)

            # Java compilation uses -d flag for output directory
            expected_cmd = ["javac", "-d", executor.temp_dir, code_path]
            mock_run.assert_called_once_with(
                expected_cmd, capture_output=True, text=True
            )

        except ImportError:
            pytest.skip("JavaExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_java_compilation_errors(self, mock_run):
        """Test Java compilation error scenarios."""
        try:
            executor = JavaExecutor()
            code_path = os.path.join(executor.temp_dir, "Test.java")

            # Test syntax error
            mock_run.return_value = Mock(
                returncode=1, stderr="Test.java:1: error: ';' expected", stdout=""
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "';' expected" in error

            # Test class not found
            mock_run.return_value = Mock(
                returncode=1, stderr="Test.java:1: error: cannot find symbol", stdout=""
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "cannot find symbol" in error

        except ImportError:
            pytest.skip("JavaExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()


class TestGoCompilationWorkflow:
    """Test Go specific compilation workflow."""

    def test_go_executor_initialization(self):
        """Test Go executor initialization."""
        try:
            executor = GoExecutor()

            assert executor.compile_cmd == ["go", "build"]
            assert executor.run_cmd == ["go", "run"]
            assert executor.file_ext == ".go"
            assert executor.use_json_input is False  # Go now uses embedded values

        except ImportError:
            pytest.skip("GoExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_go_compilation_command(self, mock_run):
        """Test Go compilation command structure."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="")

        try:
            executor = GoExecutor()

            code_path = os.path.join(executor.temp_dir, "test.go")
            success, output_path, error = executor.compile(code_path)

            # Go build command
            expected_output = code_path.replace(".go", "")
            expected_cmd = ["go", "build", "-o", expected_output, code_path]
            mock_run.assert_called_once_with(
                expected_cmd, capture_output=True, text=True
            )

        except ImportError:
            pytest.skip("GoExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_go_compilation_errors(self, mock_run):
        """Test Go compilation error scenarios."""
        try:
            executor = GoExecutor()
            code_path = os.path.join(executor.temp_dir, "test.go")

            # Test syntax error
            mock_run.return_value = Mock(
                returncode=2,
                stderr="test.go:1:1: expected 'package', found 'IDENT'",
                stdout="",
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "expected 'package'" in error

            # Test undefined variable
            mock_run.return_value = Mock(
                returncode=2, stderr="test.go:5:2: undefined: undeclaredVar", stdout=""
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "undefined: undeclaredVar" in error

        except ImportError:
            pytest.skip("GoExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()


class TestHaskellCompilationWorkflow:
    """Test Haskell specific compilation workflow."""

    def test_haskell_executor_initialization(self):
        """Test Haskell executor initialization."""
        try:
            executor = HaskellExecutor()

            assert executor.compile_cmd == ["ghc"]
            assert executor.run_cmd is None
            assert executor.file_ext == ".hs"
            assert executor.use_json_input is False

        except ImportError:
            pytest.skip("HaskellExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_haskell_compilation_command(self, mock_run):
        """Test Haskell compilation command structure."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="")

        try:
            executor = HaskellExecutor()

            code_path = os.path.join(executor.temp_dir, "test.hs")
            success, output_path, error = executor.compile(code_path)

            # GHC compilation
            expected_output = code_path.replace(".hs", "")
            expected_cmd = ["ghc", "-O0", "-o", expected_output, code_path]
            mock_run.assert_called_once_with(
                expected_cmd, capture_output=True, text=True
            )

        except ImportError:
            pytest.skip("HaskellExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()

    @patch("subprocess.run")
    def test_haskell_compilation_errors(self, mock_run):
        """Test Haskell compilation error scenarios."""
        try:
            executor = HaskellExecutor()
            code_path = os.path.join(executor.temp_dir, "test.hs")

            # Test type error
            mock_run.return_value = Mock(
                returncode=1,
                stderr="test.hs:1:1: error: Couldn't match expected type",
                stdout="",
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "Couldn't match expected type" in error

            # Test parse error
            mock_run.return_value = Mock(
                returncode=1,
                stderr="test.hs:1:1: error: parse error on input",
                stdout="",
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "parse error" in error

        except ImportError:
            pytest.skip("HaskellExecutor not available")
        finally:
            if "executor" in locals():
                executor.cleanup()


class TestCompilationExecutionIntegration:
    """Test integration between compilation and execution phases."""

    @patch("subprocess.run")
    def test_successful_compile_then_execute(self, mock_run):
        """Test successful compilation followed by execution."""
        # Mock compilation success
        compile_result = Mock(returncode=0, stderr="", stdout="")
        # Mock execution success
        execute_result = Mock(returncode=0, stderr="", stdout="42")

        mock_run.side_effect = [compile_result, execute_result]

        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc"], run_cmd=["./"], file_ext=".c", use_json_input=False
        )

        # Mock the prepare_code method
        with patch.object(executor, "prepare_code", return_value="prepared_code"):
            try:
                test_case = {"parameters": {"x": 5}, "expected": 42}
                result = executor.execute_test("test_code", test_case)

                assert result["passed"] is True  # "42" is parsed as JSON to int 42
                assert result["actual"] == 42  # JSON parsed
                assert result["expected"] == 42

                # Should have called compile then execute
                assert mock_run.call_count == 2

                # First call should be compilation
                compile_call = mock_run.call_args_list[0]
                assert "gcc" in compile_call[0][0]

                # Second call should be execution
                execute_call = mock_run.call_args_list[1]
                assert execute_call[0][0][0].endswith("test")  # Output executable

            finally:
                executor.cleanup()

    @patch("subprocess.run")
    def test_compilation_failure_prevents_execution(self, mock_run):
        """Test that compilation failure prevents execution."""
        # Mock compilation failure
        mock_run.return_value = Mock(
            returncode=1, stderr="Compilation failed", stdout=""
        )

        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc"], run_cmd=["./"], file_ext=".c", use_json_input=False
        )

        # Mock the prepare_code method
        with patch.object(executor, "prepare_code", return_value="prepared_code"):
            try:
                test_case = {"parameters": {"x": 5}, "expected": 42}
                result = executor.execute_test("test_code", test_case)

                assert result["passed"] is False
                assert "Compilation failed" in result["error"]
                assert result["actual"] is None

                # Should only have called compile, not execute
                assert mock_run.call_count == 1

                # Call should be compilation
                compile_call = mock_run.call_args_list[0]
                assert "gcc" in compile_call[0][0]

            finally:
                executor.cleanup()


class TestCompilationErrorRecovery:
    """Test compilation error recovery and handling."""

    @patch("subprocess.run")
    def test_partial_compilation_recovery(self, mock_run):
        """Test recovery from partial compilation errors."""
        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc", "-Wall", "-Wextra"],
            run_cmd=["./"],
            file_ext=".c",
            use_json_input=False,
        )

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            # Test warning (should succeed)
            mock_run.return_value = Mock(
                returncode=0, stderr="warning: unused parameter 'x'", stdout=""
            )

            success, output_path, error = executor.compile(code_path)
            assert success is True
            assert "warning" in error

            # Test error (should fail)
            mock_run.return_value = Mock(
                returncode=1, stderr="error: 'main' function not found", stdout=""
            )

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "error" in error

        finally:
            executor.cleanup()

    @patch("subprocess.run")
    def test_compilation_output_parsing(self, mock_run):
        """Test parsing of compilation output and errors."""
        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc"], run_cmd=["./"], file_ext=".c", use_json_input=False
        )

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            # Test complex error message
            complex_error = """test.c: In function 'main':
test.c:5:5: error: 'undeclared_var' undeclared (first use in this function)
test.c:5:5: note: each undeclared identifier is reported only once
test.c:6:10: warning: unused variable 'y' [-Wunused-variable]"""

            mock_run.return_value = Mock(returncode=1, stderr=complex_error, stdout="")

            success, output_path, error = executor.compile(code_path)
            assert success is False
            assert "undeclared_var" in error
            assert "warning" in error
            assert "note" in error

        finally:
            executor.cleanup()

    def test_compilation_environment_isolation(self):
        """Test that compilation environments are properly isolated."""
        executor1 = MockCompiledLanguageExecutor(["gcc"], ["./"], ".c")
        executor2 = MockCompiledLanguageExecutor(["gcc"], ["./"], ".c")

        try:
            # Different temp directories
            assert executor1.temp_dir != executor2.temp_dir

            # Create files in each
            file1 = os.path.join(executor1.temp_dir, "test.c")
            file2 = os.path.join(executor2.temp_dir, "test.c")

            with open(file1, "w") as f:
                f.write("// executor 1")
            with open(file2, "w") as f:
                f.write("// executor 2")

            # Files should exist in respective directories
            assert os.path.exists(file1)
            assert os.path.exists(file2)

            # Cleanup one should not affect the other
            executor1.cleanup()
            assert not os.path.exists(file1)
            assert os.path.exists(file2)

        finally:
            if os.path.exists(executor1.temp_dir):
                executor1.cleanup()
            if os.path.exists(executor2.temp_dir):
                executor2.cleanup()


class TestCompilationPerformance:
    """Test compilation performance and resource usage."""

    @patch("subprocess.run")
    def test_compilation_timeout_configuration(self, mock_run):
        """Test compilation with timeout configuration."""

        # Simulate slow compilation
        def slow_compile(*args, **kwargs):
            import time

            time.sleep(0.1)  # Small delay for testing
            return Mock(returncode=0, stderr="", stdout="")

        mock_run.side_effect = slow_compile

        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc"], run_cmd=["./"], file_ext=".c", use_json_input=False
        )

        try:
            code_path = os.path.join(executor.temp_dir, "test.c")

            # Compilation should succeed even with delay
            success, output_path, error = executor.compile(code_path)
            assert success is True

        finally:
            executor.cleanup()

    def test_multiple_compilation_cleanup(self):
        """Test cleanup after multiple compilation attempts."""
        executor = MockCompiledLanguageExecutor(
            compile_cmd=["gcc"], run_cmd=["./"], file_ext=".c", use_json_input=False
        )

        try:
            temp_dir = executor.temp_dir

            # Create multiple files (simulating multiple compilation attempts)
            files = []
            for i in range(5):
                file_path = os.path.join(temp_dir, f"test{i}.c")
                with open(file_path, "w") as f:
                    f.write(f"// test file {i}")
                files.append(file_path)

            # All files should exist
            for file_path in files:
                assert os.path.exists(file_path)

            # Cleanup should remove all files
            executor.cleanup()

            # Directory and all files should be gone
            assert not os.path.exists(temp_dir)
            for file_path in files:
                assert not os.path.exists(file_path)

        finally:
            if hasattr(executor, "temp_dir") and os.path.exists(executor.temp_dir):
                executor.cleanup()
