"""Tests for base executor classes and infrastructure."""

import pytest
import tempfile
import os
import shutil
import json
import subprocess
from unittest.mock import Mock, patch, MagicMock, call
from typing import Dict, Any

from eiplgrader.languages.executors.base_executors import (
    LanguageExecutor,
    CompiledLanguageExecutor,
    InterpretedLanguageExecutor,
)


class MockLanguageExecutor(LanguageExecutor):
    """Mock language executor for testing."""

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        return code

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        return {"passed": True, "actual": "mock", "expected": "mock"}

    def cleanup(self) -> None:
        pass


class MockCompiledExecutor(CompiledLanguageExecutor):
    """Mock compiled executor for testing."""

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        return code



class MockInterpretedExecutor(InterpretedLanguageExecutor):
    """Mock interpreted executor for testing."""

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        return code



class TestLanguageExecutorBase:
    """Test abstract base executor functionality."""

    def test_abstract_methods_required(self):
        """Test that abstract methods are required."""
        with pytest.raises(TypeError):
            LanguageExecutor()  # pylint: disable=abstract-class-instantiated

    def test_validate_types_provided_success(self):
        """Test successful type validation."""

        class MockExecutor(LanguageExecutor):
            def prepare_code(self, code, test_case):
                pass

            def execute_test(self, code, test_case):
                pass

            def cleanup(self):
                pass

        executor = MockExecutor()

        test_case = {
            "parameters": {"x": 5, "y": 10},
            "parameter_types": {"x": "int", "y": "int"},
            "expected": 15,
            "expected_type": "int",
        }

        # Should not raise any exception
        executor.validate_types_provided(test_case)

    def test_validate_types_provided_missing_parameter_types(self):
        """Test type validation with missing parameter_types."""

        class MockExecutor(LanguageExecutor):
            def prepare_code(self, code, test_case):
                pass

            def execute_test(self, code, test_case):
                pass

            def cleanup(self):
                pass

        executor = MockExecutor()

        test_case = {"parameters": {"x": 5}, "expected": 15, "expected_type": "int"}

        with pytest.raises(ValueError, match="parameter_types not provided"):
            executor.validate_types_provided(test_case)

    def test_validate_types_provided_missing_expected_type(self):
        """Test type validation with missing expected_type."""

        class MockExecutor(LanguageExecutor):
            def prepare_code(self, code, test_case):
                pass

            def execute_test(self, code, test_case):
                pass

            def cleanup(self):
                pass

        executor = MockExecutor()

        test_case = {
            "parameters": {"x": 5},
            "parameter_types": {"x": "int"},
            "expected": 15,
        }

        with pytest.raises(ValueError, match="expected_type not provided"):
            executor.validate_types_provided(test_case)

    def test_validate_types_provided_missing_parameter_type(self):
        """Test type validation with missing specific parameter type."""

        class MockExecutor(LanguageExecutor):
            def prepare_code(self, code, test_case):
                pass

            def execute_test(self, code, test_case):
                pass

            def cleanup(self):
                pass

        executor = MockExecutor()

        test_case = {
            "parameters": {"x": 5, "y": 10},
            "parameter_types": {"x": "int"},  # Missing y
            "expected": 15,
            "expected_type": "int",
        }

        with pytest.raises(ValueError, match="parameter_types\\['y'\\] not provided"):
            executor.validate_types_provided(test_case)

    def test_infer_type_basic_types(self):
        """Test type inference for basic types."""

        class MockExecutor(LanguageExecutor):
            def prepare_code(self, code, test_case):
                pass

            def execute_test(self, code, test_case):
                pass

            def cleanup(self):
                pass

        executor = MockExecutor()

        assert executor.infer_type(True) == "bool"
        assert executor.infer_type(False) == "bool"
        assert executor.infer_type(42) == "int"
        assert executor.infer_type(3.14) == "double"
        assert executor.infer_type("hello") == "string"

    def test_infer_type_lists(self):
        """Test type inference for lists."""

        class MockExecutor(LanguageExecutor):
            def prepare_code(self, code, test_case):
                pass

            def execute_test(self, code, test_case):
                pass

            def cleanup(self):
                pass

        executor = MockExecutor()

        assert executor.infer_type([1, 2, 3]) == "List[int]"
        assert executor.infer_type([1.0, 2.0]) == "List[double]"
        assert executor.infer_type(["a", "b"]) == "List[string]"
        assert executor.infer_type([]) == "List"
        assert executor.infer_type([True, False]) == "List"  # Mixed types

    def test_infer_type_unknown(self):
        """Test type inference for unknown types."""

        class MockExecutor(LanguageExecutor):
            def prepare_code(self, code, test_case):
                pass

            def execute_test(self, code, test_case):
                pass

            def cleanup(self):
                pass

        executor = MockExecutor()

        assert executor.infer_type({"key": "value"}) == "unknown"
        assert executor.infer_type(None) == "unknown"


class TestCompiledLanguageExecutor:
    """Test compiled language executor base class."""

    def setup_method(self):
        """Set up test fixtures."""
        self.compile_cmd = ["gcc"]
        self.run_cmd = ["./"]
        self.file_ext = ".c"
        self.executor = MockCompiledExecutor(
            compile_cmd=self.compile_cmd,
            run_cmd=self.run_cmd,
            file_ext=self.file_ext,
            use_json_input=False,
        )

    def teardown_method(self):
        """Clean up after tests."""
        if hasattr(self.executor, "temp_dir"):
            self.executor.cleanup()

    def test_initialization(self):
        """Test executor initialization."""
        assert self.executor.compile_cmd == ["gcc"]
        assert self.executor.run_cmd == ["./"]
        assert self.executor.file_ext == ".c"
        assert self.executor.use_json_input is False
        assert os.path.exists(self.executor.temp_dir)
        assert os.path.isdir(self.executor.temp_dir)

    def test_initialization_with_json_input(self):
        """Test executor initialization with JSON input."""
        executor = MockCompiledExecutor(
            compile_cmd=["go", "build"],
            run_cmd=["./"],
            file_ext=".go",
            use_json_input=True,
        )

        assert executor.use_json_input is True
        executor.cleanup()

    def test_cleanup(self):
        """Test temporary directory cleanup."""
        temp_dir = self.executor.temp_dir
        assert os.path.exists(temp_dir)

        # Create a test file in temp dir
        test_file = os.path.join(temp_dir, "test.txt")
        with open(test_file, "w") as f:
            f.write("test content")

        assert os.path.exists(test_file)

        # Cleanup should remove everything
        self.executor.cleanup()
        assert not os.path.exists(temp_dir)

    def test_cleanup_nonexistent_dir(self):
        """Test cleanup when directory doesn't exist."""
        self.executor.cleanup()  # First cleanup
        # Second cleanup should not raise error
        self.executor.cleanup()

    @patch("subprocess.run")
    def test_compile_success(self, mock_run):
        """Test successful compilation."""
        mock_run.return_value = Mock(returncode=0, stderr="")

        code_path = "/tmp/test.c"
        success, output_path, error = self.executor.compile(code_path)

        assert success is True
        assert output_path == "/tmp/test"
        assert error == ""

        mock_run.assert_called_once_with(
            ["gcc", "-o", "/tmp/test", "/tmp/test.c"], capture_output=True, text=True
        )

    @patch("subprocess.run")
    def test_compile_failure(self, mock_run):
        """Test compilation failure."""
        mock_run.return_value = Mock(returncode=1, stderr="Compilation error")

        code_path = "/tmp/test.c"
        success, output_path, error = self.executor.compile(code_path)

        assert success is False
        assert output_path == "/tmp/test"
        assert error == "Compilation error"

    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.compile"
    )
    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_compilation_failure(
        self, mock_run, mock_prepare, mock_compile
    ):  # pylint: disable=unused-argument
        """Test execute_test when compilation fails."""
        mock_prepare.return_value = "prepared_code"
        mock_compile.return_value = (False, "/tmp/test", "Compilation failed")

        test_case = {"parameters": {"x": 5}, "expected": 10}
        result = self.executor.execute_test("def test(): pass", test_case)

        assert result["passed"] is False
        assert "Compilation failed" in result["error"]
        assert result["actual"] is None
        assert result["expected"] == 10

    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.compile"
    )
    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_success_no_json(self, mock_run, mock_prepare, mock_compile):
        """Test successful execution without JSON input."""
        mock_prepare.return_value = "prepared_code"
        mock_compile.return_value = (True, "/tmp/test", "")
        mock_run.return_value = Mock(returncode=0, stdout='{"result": 42}', stderr="")

        test_case = {"parameters": {"x": 5}, "expected": {"result": 42}}
        result = self.executor.execute_test("code", test_case)

        assert result["passed"] is True
        assert result["actual"] == {"result": 42}
        assert result["expected"] == {"result": 42}
        assert result["output"] == '{"result": 42}'

        # Should not pass input since use_json_input=False
        mock_run.assert_called_once_with(
            ["/tmp/test"], capture_output=True, text=True, timeout=30
        )

    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.compile"
    )
    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_success_with_json(self, mock_run, mock_prepare, mock_compile):
        """Test successful execution with JSON input."""
        # Create executor with JSON input
        executor = MockCompiledExecutor(
            compile_cmd=["go", "build"],
            run_cmd=["./"],
            file_ext=".go",
            use_json_input=True,
        )

        mock_prepare.return_value = "prepared_code"
        mock_compile.return_value = (True, "/tmp/test", "")
        mock_run.return_value = Mock(returncode=0, stdout="15", stderr="")

        test_case = {"parameters": {"x": 5, "y": 10}, "expected": 15}
        result = executor.execute_test("code", test_case)

        assert result["passed"] is True
        assert result["actual"] == 15  # JSON successfully parses "15" as integer

        # Should pass JSON input
        expected_json = '{"x": 5, "y": 10}'
        mock_run.assert_called_once_with(
            ["/tmp/test"],
            input=expected_json,
            capture_output=True,
            text=True,
            timeout=30,
        )

        executor.cleanup()

    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.compile"
    )
    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_runtime_error(self, mock_run, mock_prepare, mock_compile):
        """Test execution with runtime error."""
        mock_prepare.return_value = "prepared_code"
        mock_compile.return_value = (True, "/tmp/test", "")
        mock_run.return_value = Mock(
            returncode=1, stdout="", stderr="Runtime error occurred"
        )

        test_case = {"parameters": {"x": 5}, "expected": 10}
        result = self.executor.execute_test("code", test_case)

        assert result["passed"] is False
        assert "Runtime error: Runtime error occurred" in result["error"]
        assert result["actual"] is None

    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.compile"
    )
    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_timeout(self, mock_run, mock_prepare, mock_compile):
        """Test execution timeout."""
        mock_prepare.return_value = "prepared_code"
        mock_compile.return_value = (True, "/tmp/test", "")
        mock_run.side_effect = subprocess.TimeoutExpired("cmd", 30)

        test_case = {"parameters": {"x": 5}, "expected": 10}
        result = self.executor.execute_test("code", test_case)

        assert result["passed"] is False
        assert result["error"] == "Execution timeout"
        assert result["actual"] is None

    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.compile"
    )
    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_custom_timeout(self, mock_run, mock_prepare, mock_compile):
        """Test execution with custom timeout."""
        mock_prepare.return_value = "prepared_code"
        mock_compile.return_value = (True, "/tmp/test", "")
        mock_run.return_value = Mock(returncode=0, stdout="42", stderr="")

        test_case = {"parameters": {"x": 5}, "expected": 42, "timeout": 60}
        result = self.executor.execute_test("code", test_case)

        # Should use custom timeout
        mock_run.assert_called_once_with(
            ["/tmp/test"], capture_output=True, text=True, timeout=60
        )

    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.compile"
    )
    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_json_parsing(self, mock_run, mock_prepare, mock_compile):
        """Test JSON parsing in execution results."""
        mock_prepare.return_value = "prepared_code"
        mock_compile.return_value = (True, "/tmp/test", "")

        # Test valid JSON
        mock_run.return_value = Mock(returncode=0, stdout='{"value": 42}', stderr="")
        test_case = {"parameters": {}, "expected": {"value": 42}}
        result = self.executor.execute_test("code", test_case)
        assert result["actual"] == {"value": 42}

        # Test invalid JSON
        mock_run.return_value = Mock(returncode=0, stdout="not json", stderr="")
        test_case = {"parameters": {}, "expected": "not json"}
        result = self.executor.execute_test("code", test_case)
        assert result["actual"] == "not json"

    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.compile"
    )
    @patch(
        "eiplgrader.languages.executors.base_executors.CompiledLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_exception_handling(
        self, mock_run, mock_prepare, mock_compile
    ):
        """Test general exception handling during execution."""
        mock_prepare.return_value = "prepared_code"
        mock_compile.return_value = (True, "/tmp/test", "")
        mock_run.side_effect = OSError("System error")

        test_case = {"parameters": {"x": 5}, "expected": 10}
        result = self.executor.execute_test("code", test_case)

        assert result["passed"] is False
        assert "System error" in result["error"]
        assert result["actual"] is None


class TestInterpretedLanguageExecutor:
    """Test interpreted language executor base class."""

    def setup_method(self):
        """Set up test fixtures."""
        self.interpreter_cmd = ["python3"]
        self.file_ext = ".py"
        self.executor = MockInterpretedExecutor(
            interpreter_cmd=self.interpreter_cmd, file_ext=self.file_ext
        )

    def teardown_method(self):
        """Clean up after tests."""
        if hasattr(self.executor, "temp_dir"):
            self.executor.cleanup()

    def test_initialization(self):
        """Test executor initialization."""
        assert self.executor.interpreter_cmd == ["python3"]
        assert self.executor.file_ext == ".py"
        assert os.path.exists(self.executor.temp_dir)
        assert os.path.isdir(self.executor.temp_dir)

    def test_validate_or_infer_types_with_types(self):
        """Test type validation when types are provided."""
        test_case = {
            "parameters": {"x": 5, "y": 10},
            "parameter_types": {"x": "int", "y": "int"},
            "expected": 15,
            "expected_type": "int",
        }

        result = self.executor.validate_or_infer_types(test_case)

        assert result["parameter_types"] == {"x": "int", "y": "int"}
        assert result["expected_type"] == "int"

    def test_validate_or_infer_types_without_types(self):
        """Test type inference when types are not provided."""
        test_case = {"parameters": {"x": 5, "y": "hello"}, "expected": [1, 2, 3]}

        result = self.executor.validate_or_infer_types(test_case)

        assert result["parameter_types"] == {"x": "int", "y": "string"}
        assert result["expected_type"] == "List[int]"

    def test_validate_or_infer_types_partial_types(self):
        """Test type inference with partial type information."""
        test_case = {
            "parameters": {"x": 5, "y": 10},
            "parameter_types": {"x": "int"},  # Missing y
            "expected": 15,
        }

        result = self.executor.validate_or_infer_types(test_case)

        # Should preserve existing types and infer missing ones
        assert result["parameter_types"]["x"] == "int"
        assert "y" in result["parameter_types"]  # Should be inferred
        assert result["expected_type"] == "int"

    @patch(
        "eiplgrader.languages.executors.base_executors.InterpretedLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_success(self, mock_run, mock_prepare):
        """Test successful execution."""
        mock_prepare.return_value = "prepared_code"
        mock_run.return_value = Mock(returncode=0, stdout='{"result": 42}', stderr="")

        test_case = {"parameters": {"x": 5}, "expected": {"result": 42}}
        result = self.executor.execute_test("code", test_case)

        assert result["passed"] is True
        assert result["actual"] == {"result": 42}
        assert result["expected"] == {"result": 42}
        assert result["output"] == '{"result": 42}'

        # Check that correct command was called
        mock_run.assert_called_once()
        args, kwargs = mock_run.call_args
        assert args[0][:2] == ["python3"]  # First part should be interpreter
        assert kwargs["capture_output"] is True
        assert kwargs["text"] is True
        assert kwargs["timeout"] == 30

    @patch(
        "eiplgrader.languages.executors.base_executors.InterpretedLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_runtime_error(self, mock_run, mock_prepare):
        """Test execution with runtime error."""
        mock_prepare.return_value = "prepared_code"
        mock_run.return_value = Mock(
            returncode=1, stdout="", stderr="SyntaxError: invalid syntax"
        )

        test_case = {"parameters": {"x": 5}, "expected": 10}
        result = self.executor.execute_test("code", test_case)

        assert result["passed"] is False
        assert "Runtime error: SyntaxError: invalid syntax" in result["error"]
        assert result["actual"] is None

    @patch(
        "eiplgrader.languages.executors.base_executors.InterpretedLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_timeout(self, mock_run, mock_prepare):
        """Test execution timeout."""
        mock_prepare.return_value = "prepared_code"
        mock_run.side_effect = subprocess.TimeoutExpired("cmd", 30)

        test_case = {"parameters": {"x": 5}, "expected": 10}
        result = self.executor.execute_test("code", test_case)

        assert result["passed"] is False
        assert result["error"] == "Execution timeout"
        assert result["actual"] is None

    @patch(
        "eiplgrader.languages.executors.base_executors.InterpretedLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_custom_timeout(self, mock_run, mock_prepare):
        """Test execution with custom timeout."""
        mock_prepare.return_value = "prepared_code"
        mock_run.return_value = Mock(returncode=0, stdout="42", stderr="")

        test_case = {"parameters": {"x": 5}, "expected": 42, "timeout": 120}
        result = self.executor.execute_test("code", test_case)

        # Should use custom timeout
        mock_run.assert_called_once()
        _, kwargs = mock_run.call_args
        assert kwargs["timeout"] == 120

    def test_cleanup(self):
        """Test temporary directory cleanup."""
        temp_dir = self.executor.temp_dir
        assert os.path.exists(temp_dir)

        # Create a test file in temp dir
        test_file = os.path.join(temp_dir, "test.py")
        with open(test_file, "w") as f:
            f.write("print('test')")

        assert os.path.exists(test_file)

        # Cleanup should remove everything
        self.executor.cleanup()
        assert not os.path.exists(temp_dir)

    @patch(
        "eiplgrader.languages.executors.base_executors.InterpretedLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_file_creation_and_cleanup(self, mock_run, mock_prepare):
        """Test that temporary files are created and cleaned up properly."""
        mock_prepare.return_value = "print('test code')"
        mock_run.return_value = Mock(returncode=0, stdout="42", stderr="")

        temp_dir = self.executor.temp_dir

        test_case = {"parameters": {"x": 5}, "expected": 42}
        result = self.executor.execute_test("code", test_case)

        # File should be created during execution
        expected_file = os.path.join(temp_dir, "test.py")

        # Check that subprocess.run was called with the correct file path
        mock_run.assert_called_once()
        args, kwargs = mock_run.call_args
        called_file_path = args[0][-1]  # Last argument should be the file path
        assert called_file_path == expected_file

    @patch(
        "eiplgrader.languages.executors.base_executors.InterpretedLanguageExecutor.prepare_code"
    )
    @patch("subprocess.run")
    def test_execute_test_json_parsing_edge_cases(self, mock_run, mock_prepare):
        """Test JSON parsing edge cases."""
        mock_prepare.return_value = "prepared_code"

        # Test whitespace around JSON
        mock_run.return_value = Mock(
            returncode=0, stdout='  {"value": 42}  \n', stderr=""
        )
        test_case = {"parameters": {}, "expected": {"value": 42}}
        result = self.executor.execute_test("code", test_case)
        assert result["actual"] == {"value": 42}

        # Test empty output
        mock_run.return_value = Mock(returncode=0, stdout="", stderr="")
        test_case = {"parameters": {}, "expected": ""}
        result = self.executor.execute_test("code", test_case)
        assert result["actual"] == ""

        # Test numeric output
        mock_run.return_value = Mock(returncode=0, stdout="42", stderr="")
        test_case = {"parameters": {}, "expected": 42}
        result = self.executor.execute_test("code", test_case)
        assert result["actual"] == 42  # JSON successfully parses "42" as integer
        assert result["passed"] is True  # 42 == 42


class TestExecutorComparison:
    """Test differences between compiled and interpreted executors."""

    def test_executor_type_differences(self):
        """Test key differences between executor types."""
        compiled = MockCompiledExecutor(["gcc"], ["./"], ".c")
        interpreted = MockInterpretedExecutor(["python3"], ".py")

        try:
            # Both should have temp directories
            assert os.path.exists(compiled.temp_dir)
            assert os.path.exists(interpreted.temp_dir)

            # Compiled should have compile method
            assert hasattr(compiled, "compile")
            assert not hasattr(interpreted, "compile")

            # Interpreted should have type inference
            assert hasattr(interpreted, "validate_or_infer_types")
            assert not hasattr(compiled, "validate_or_infer_types")

            # Both should have same abstract methods
            for method in ["prepare_code", "execute_test", "cleanup"]:
                assert hasattr(compiled, method)
                assert hasattr(interpreted, method)

        finally:
            compiled.cleanup()
            interpreted.cleanup()

    def test_temp_directory_isolation(self):
        """Test that different executors get different temp directories."""
        executor1 = MockCompiledExecutor(["gcc"], ["./"], ".c")
        executor2 = MockCompiledExecutor(["gcc"], ["./"], ".c")
        executor3 = MockInterpretedExecutor(["python3"], ".py")

        try:
            # All should have different temp directories
            assert executor1.temp_dir != executor2.temp_dir
            assert executor1.temp_dir != executor3.temp_dir
            assert executor2.temp_dir != executor3.temp_dir

            # All should exist
            assert os.path.exists(executor1.temp_dir)
            assert os.path.exists(executor2.temp_dir)
            assert os.path.exists(executor3.temp_dir)

        finally:
            executor1.cleanup()
            executor2.cleanup()
            executor3.cleanup()


class TestExecutorResourceManagement:
    """Test resource management and cleanup."""

    def test_multiple_cleanup_calls(self):
        """Test that multiple cleanup calls are safe."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        temp_dir = executor.temp_dir

        assert os.path.exists(temp_dir)

        # First cleanup
        executor.cleanup()
        assert not os.path.exists(temp_dir)

        # Second cleanup should not raise error
        executor.cleanup()  # Should be safe

    def test_cleanup_with_open_files(self):
        """Test cleanup behavior when files might be open."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
        temp_dir = executor.temp_dir

        # Create a file
        test_file = os.path.join(temp_dir, "test.txt")
        with open(test_file, "w") as f:
            f.write("test content")

        assert os.path.exists(test_file)

        # Cleanup should remove file
        executor.cleanup()
        assert not os.path.exists(temp_dir)

    def test_temp_directory_permissions(self):
        """Test that temp directories have correct permissions."""
        executor = MockCompiledExecutor(["gcc"], ["./"], ".c")

        try:
            temp_dir = executor.temp_dir

            # Should be able to read, write, and execute
            assert os.access(temp_dir, os.R_OK)
            assert os.access(temp_dir, os.W_OK)
            assert os.access(temp_dir, os.X_OK)

            # Should be able to create files
            test_file = os.path.join(temp_dir, "test.txt")
            with open(test_file, "w") as f:
                f.write("test")

            assert os.path.exists(test_file)

        finally:
            executor.cleanup()

    def test_concurrent_executor_usage(self):
        """Test that multiple executors can be used concurrently."""
        import threading
        import time

        results = []
        errors = []

        def create_and_cleanup_executor(executor_id):
            try:
                executor = MockCompiledExecutor(["gcc"], ["./"], ".c")
                temp_dir = executor.temp_dir

                # Simulate some work
                test_file = os.path.join(temp_dir, f"test_{executor_id}.txt")
                with open(test_file, "w") as f:
                    f.write(f"executor {executor_id}")

                time.sleep(0.1)  # Simulate work

                assert os.path.exists(test_file)
                executor.cleanup()
                assert not os.path.exists(temp_dir)

                results.append(executor_id)
            except Exception as e:
                errors.append((executor_id, str(e)))

        # Run multiple executors concurrently
        threads = []
        for i in range(5):
            thread = threading.Thread(target=create_and_cleanup_executor, args=(i,))
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

        # All should succeed
        assert len(errors) == 0
        assert len(results) == 5
        assert set(results) == {0, 1, 2, 3, 4}
