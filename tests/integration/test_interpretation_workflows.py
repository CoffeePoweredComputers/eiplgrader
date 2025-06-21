"""Tests for interpretation workflows across interpreted languages."""

import pytest
import tempfile
import os
import shutil
import json
import subprocess
from unittest.mock import Mock, patch, MagicMock
from typing import Dict, Any

from eiplgrader.languages.executors.base_executors import InterpretedLanguageExecutor
from eiplgrader.languages.executors.python_executor import PythonExecutor
from eiplgrader.languages.executors.javascript_executor import JavaScriptExecutor


class TestInterpretationWorkflowBase:
    """Test base interpretation workflow functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
    
    def teardown_method(self):
        """Clean up test fixtures."""
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)
    
    @patch('subprocess.run')
    def test_interpretation_success_workflow(self, mock_run):
        """Test successful interpretation workflow."""
        mock_run.return_value = Mock(
            returncode=0, 
            stdout='{"result": 42}',
            stderr=""
        )
        
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            # Mock prepare_code method
            with patch.object(executor, 'prepare_code', return_value="print('test')"):
                test_case = {"parameters": {"x": 5}, "expected": {"result": 42}}
                result = executor.execute_test("test_code", test_case)
                
                assert result["passed"] is True
                assert result["actual"] == {"result": 42}
                assert result["expected"] == {"result": 42}
                assert result["output"] == '{"result": 42}'
                
                # Verify correct command was called
                mock_run.assert_called_once()
                args, kwargs = mock_run.call_args
                assert args[0][0] == "python3"
                assert kwargs["capture_output"] is True
                assert kwargs["text"] is True
                assert kwargs["timeout"] == 30
        
        finally:
            executor.cleanup()
    
    @patch('subprocess.run')
    def test_interpretation_runtime_error_workflow(self, mock_run):
        """Test interpretation with runtime error."""
        mock_run.return_value = Mock(
            returncode=1,
            stdout="",
            stderr="NameError: name 'undefined_var' is not defined"
        )
        
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            with patch.object(executor, 'prepare_code', return_value="print(undefined_var)"):
                test_case = {"parameters": {"x": 5}, "expected": 42}
                result = executor.execute_test("test_code", test_case)
                
                assert result["passed"] is False
                assert "Runtime error:" in result["error"]
                assert "undefined_var" in result["error"]
                assert result["actual"] is None
        
        finally:
            executor.cleanup()
    
    @patch('subprocess.run')
    def test_interpretation_timeout_workflow(self, mock_run):
        """Test interpretation timeout handling."""
        mock_run.side_effect = subprocess.TimeoutExpired("python3", 30)
        
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            with patch.object(executor, 'prepare_code', return_value="while True: pass"):
                test_case = {"parameters": {"x": 5}, "expected": 42}
                result = executor.execute_test("test_code", test_case)
                
                assert result["passed"] is False
                assert result["error"] == "Execution timeout"
                assert result["actual"] is None
        
        finally:
            executor.cleanup()
    
    @patch('subprocess.run')
    def test_interpretation_custom_timeout(self, mock_run):
        """Test interpretation with custom timeout."""
        mock_run.return_value = Mock(returncode=0, stdout="42", stderr="")
        
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            with patch.object(executor, 'prepare_code', return_value="print(42)"):
                test_case = {"parameters": {"x": 5}, "expected": 42, "timeout": 120}
                result = executor.execute_test("test_code", test_case)
                
                # Should use custom timeout
                mock_run.assert_called_once()
                _, kwargs = mock_run.call_args
                assert kwargs["timeout"] == 120
        
        finally:
            executor.cleanup()
    
    def test_type_inference_workflow(self):
        """Test type inference in interpretation workflow."""
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            # Test case without types
            test_case = {
                "parameters": {"x": 5, "y": "hello", "z": [1, 2, 3]},
                "expected": {"success": True}
            }
            
            # Type inference should add types
            result_case = executor.validate_or_infer_types(test_case)
            
            assert result_case["parameter_types"]["x"] == "int"
            assert result_case["parameter_types"]["y"] == "string"
            assert result_case["parameter_types"]["z"] == "List[int]"
            assert result_case["expected_type"] == "unknown"  # dict not handled
        
        finally:
            executor.cleanup()
    
    def test_file_creation_and_execution_workflow(self):
        """Test file creation during interpretation workflow."""
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            temp_dir = executor.temp_dir
            expected_file_path = os.path.join(temp_dir, "test.py")
            
            with patch('subprocess.run') as mock_run:
                mock_run.return_value = Mock(returncode=0, stdout="42", stderr="")
                
                with patch.object(executor, 'prepare_code', return_value="print(42)"):
                    test_case = {"parameters": {"x": 5}, "expected": 42}
                    result = executor.execute_test("test_code", test_case)
                    
                    # Verify file path was used in subprocess call
                    mock_run.assert_called_once()
                    args, kwargs = mock_run.call_args
                    called_file_path = args[0][-1]  # Last argument should be file path
                    assert called_file_path == expected_file_path
        
        finally:
            executor.cleanup()


class TestPythonInterpretationWorkflow:
    """Test Python-specific interpretation workflow."""
    
    def test_python_executor_initialization(self):
        """Test Python executor initialization."""
        try:
            executor = PythonExecutor()
            
            assert executor.interpreter_cmd == ["python3"]
            assert executor.file_ext == ".py"
            assert hasattr(executor, 'temp_dir')
            assert os.path.exists(executor.temp_dir)
        
        except ImportError:
            pytest.skip("PythonExecutor not available")
        finally:
            if 'executor' in locals():
                executor.cleanup()
    
    @patch('subprocess.run')
    def test_python_execution_command(self, mock_run):
        """Test Python execution command structure."""
        mock_run.return_value = Mock(returncode=0, stdout="42", stderr="")
        
        try:
            executor = PythonExecutor()
            
            with patch.object(executor, 'prepare_code', return_value="print(42)"):
                test_case = {"parameters": {"x": 5}, "expected": 42}
                result = executor.execute_test("test_code", test_case)
                
                # Verify python3 was called
                mock_run.assert_called_once()
                args, kwargs = mock_run.call_args
                assert args[0][0] == "python3"
                assert args[0][1].endswith(".py")
        
        except ImportError:
            pytest.skip("PythonExecutor not available")
        finally:
            if 'executor' in locals():
                executor.cleanup()
    
    @patch('subprocess.run')
    def test_python_error_scenarios(self, mock_run):
        """Test various Python error scenarios."""
        try:
            executor = PythonExecutor()
            
            # Test syntax error
            mock_run.return_value = Mock(
                returncode=1,
                stdout="",
                stderr="  File \"test.py\", line 1\n    print(\n         ^\nSyntaxError: unexpected EOF while parsing"
            )
            
            with patch.object(executor, 'prepare_code', return_value="print("):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("print(", test_case)
                
                assert result["passed"] is False
                assert "SyntaxError" in result["error"]
            
            # Test runtime error
            mock_run.return_value = Mock(
                returncode=1,
                stdout="",
                stderr="Traceback (most recent call last):\n  File \"test.py\", line 1, in <module>\n    print(x)\nNameError: name 'x' is not defined"
            )
            
            with patch.object(executor, 'prepare_code', return_value="print(x)"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("print(x)", test_case)
                
                assert result["passed"] is False
                assert "NameError" in result["error"]
        
        except ImportError:
            pytest.skip("PythonExecutor not available")
        finally:
            if 'executor' in locals():
                executor.cleanup()
    
    def test_python_type_inference_specifics(self):
        """Test Python-specific type inference."""
        try:
            executor = PythonExecutor()
            
            # Test various Python types
            test_case = {
                "parameters": {
                    "int_val": 42,
                    "float_val": 3.14,
                    "str_val": "hello",
                    "bool_val": True,
                    "list_int": [1, 2, 3],
                    "list_str": ["a", "b"],
                    "list_float": [1.0, 2.0],
                    "empty_list": []
                },
                "expected": "result"
            }
            
            result_case = executor.validate_or_infer_types(test_case)
            
            assert result_case["parameter_types"]["int_val"] == "int"
            assert result_case["parameter_types"]["float_val"] == "double"
            assert result_case["parameter_types"]["str_val"] == "string"
            assert result_case["parameter_types"]["bool_val"] == "bool"
            assert result_case["parameter_types"]["list_int"] == "List[int]"
            assert result_case["parameter_types"]["list_str"] == "List[string]"
            assert result_case["parameter_types"]["list_float"] == "List[double]"
            assert result_case["parameter_types"]["empty_list"] == "List"
            assert result_case["expected_type"] == "string"
        
        except ImportError:
            pytest.skip("PythonExecutor not available")
        finally:
            if 'executor' in locals():
                executor.cleanup()


class TestJavaScriptInterpretationWorkflow:
    """Test JavaScript-specific interpretation workflow."""
    
    def test_javascript_executor_initialization(self):
        """Test JavaScript executor initialization."""
        try:
            executor = JavaScriptExecutor()
            
            assert executor.interpreter_cmd == ["node"]
            assert executor.file_ext == ".js"
            assert hasattr(executor, 'temp_dir')
            assert os.path.exists(executor.temp_dir)
        
        except ImportError:
            pytest.skip("JavaScriptExecutor not available")
        finally:
            if 'executor' in locals():
                executor.cleanup()
    
    @patch('subprocess.run')
    def test_javascript_execution_command(self, mock_run):
        """Test JavaScript execution command structure."""
        mock_run.return_value = Mock(returncode=0, stdout="42", stderr="")
        
        try:
            executor = JavaScriptExecutor()
            
            with patch.object(executor, 'prepare_code', return_value="console.log(42)"):
                test_case = {"parameters": {"x": 5}, "expected": 42}
                result = executor.execute_test("test_code", test_case)
                
                # Verify node was called
                mock_run.assert_called_once()
                args, kwargs = mock_run.call_args
                assert args[0][0] == "node"
                assert args[0][1].endswith(".js")
        
        except ImportError:
            pytest.skip("JavaScriptExecutor not available")
        finally:
            if 'executor' in locals():
                executor.cleanup()
    
    @patch('subprocess.run')
    def test_javascript_error_scenarios(self, mock_run):
        """Test various JavaScript error scenarios."""
        try:
            executor = JavaScriptExecutor()
            
            # Test syntax error
            mock_run.return_value = Mock(
                returncode=1,
                stdout="",
                stderr="/tmp/test.js:1\nconsole.log(\n           ^\nSyntaxError: missing ) after argument list"
            )
            
            with patch.object(executor, 'prepare_code', return_value="console.log("):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("console.log(", test_case)
                
                assert result["passed"] is False
                assert "SyntaxError" in result["error"]
            
            # Test reference error
            mock_run.return_value = Mock(
                returncode=1,
                stdout="",
                stderr="/tmp/test.js:1\nconsole.log(undefinedVar);\n            ^\nReferenceError: undefinedVar is not defined"
            )
            
            with patch.object(executor, 'prepare_code', return_value="console.log(undefinedVar)"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("console.log(undefinedVar)", test_case)
                
                assert result["passed"] is False
                assert "ReferenceError" in result["error"]
        
        except ImportError:
            pytest.skip("JavaScriptExecutor not available")
        finally:
            if 'executor' in locals():
                executor.cleanup()


class TestInterpretationOutputParsing:
    """Test output parsing in interpretation workflows."""
    
    @patch('subprocess.run')
    def test_json_output_parsing(self, mock_run):
        """Test parsing of JSON output from interpreters."""
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            with patch.object(executor, 'prepare_code', return_value="import json; print(json.dumps({'key': 'value'}))"):
                # Test valid JSON
                mock_run.return_value = Mock(
                    returncode=0,
                    stdout='{"key": "value"}',
                    stderr=""
                )
                
                test_case = {"parameters": {}, "expected": {"key": "value"}}
                result = executor.execute_test("test", test_case)
                
                assert result["actual"] == {"key": "value"}
                assert result["passed"] is True
                
                # Test invalid JSON
                mock_run.return_value = Mock(
                    returncode=0,
                    stdout='not valid json',
                    stderr=""
                )
                
                test_case = {"parameters": {}, "expected": "not valid json"}
                result = executor.execute_test("test", test_case)
                
                assert result["actual"] == "not valid json"
                assert result["passed"] is True
        
        finally:
            executor.cleanup()
    
    @patch('subprocess.run')
    def test_numeric_output_parsing(self, mock_run):
        """Test parsing of numeric output."""
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            with patch.object(executor, 'prepare_code', return_value="print(42)"):
                # Test integer output
                mock_run.return_value = Mock(
                    returncode=0,
                    stdout='42',
                    stderr=""
                )
                
                test_case = {"parameters": {}, "expected": 42}
                result = executor.execute_test("test", test_case)
                
                # Output is string, expected is int - should not match
                assert result["actual"] == "42"
                assert result["expected"] == 42
                assert result["passed"] is False
                
                # Test with JSON numeric output
                mock_run.return_value = Mock(
                    returncode=0,
                    stdout='42',  # This could be parsed as JSON
                    stderr=""
                )
                
                test_case = {"parameters": {}, "expected": "42"}  # String expected
                result = executor.execute_test("test", test_case)
                
                assert result["actual"] == "42"
                assert result["passed"] is True
        
        finally:
            executor.cleanup()
    
    @patch('subprocess.run')
    def test_empty_output_handling(self, mock_run):
        """Test handling of empty output."""
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            with patch.object(executor, 'prepare_code', return_value="pass"):
                # Test empty output
                mock_run.return_value = Mock(
                    returncode=0,
                    stdout='',
                    stderr=""
                )
                
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("test", test_case)
                
                assert result["actual"] == ""
                assert result["passed"] is True
                
                # Test whitespace-only output
                mock_run.return_value = Mock(
                    returncode=0,
                    stdout='   \n\t  ',
                    stderr=""
                )
                
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("test", test_case)
                
                # Should strip whitespace for JSON parsing attempt
                assert result["actual"] in ["", "   \n\t  "]  # Depends on implementation
        
        finally:
            executor.cleanup()


class TestInterpretationResourceManagement:
    """Test resource management in interpretation workflows."""
    
    def test_temporary_file_lifecycle(self):
        """Test temporary file creation and cleanup lifecycle."""
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        temp_dir = executor.temp_dir
        assert os.path.exists(temp_dir)
        
        # File should be created during execution
        with patch('subprocess.run') as mock_run:
            mock_run.return_value = Mock(returncode=0, stdout="", stderr="")
            
            with patch.object(executor, 'prepare_code', return_value="print('test')"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("test", test_case)
                
                # After execution, temp dir should still exist
                assert os.path.exists(temp_dir)
        
        # After cleanup, temp dir should be gone
        executor.cleanup()
        assert not os.path.exists(temp_dir)
    
    def test_multiple_execution_file_reuse(self):
        """Test that multiple executions reuse the same file path."""
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            expected_file_path = os.path.join(executor.temp_dir, "test.py")
            
            with patch('subprocess.run') as mock_run:
                mock_run.return_value = Mock(returncode=0, stdout="", stderr="")
                
                # First execution
                with patch.object(executor, 'prepare_code', return_value="print(1)"):
                    test_case = {"parameters": {}, "expected": ""}
                    result1 = executor.execute_test("test1", test_case)
                    
                    call1 = mock_run.call_args_list[0]
                    file1 = call1[0][0][-1]
                    assert file1 == expected_file_path
                
                # Second execution
                with patch.object(executor, 'prepare_code', return_value="print(2)"):
                    test_case = {"parameters": {}, "expected": ""}
                    result2 = executor.execute_test("test2", test_case)
                    
                    call2 = mock_run.call_args_list[1]
                    file2 = call2[0][0][-1]
                    assert file2 == expected_file_path
                
                # Same file path should be reused
                assert file1 == file2
        
        finally:
            executor.cleanup()
    
    def test_concurrent_interpretation_isolation(self):
        """Test that concurrent interpreters are properly isolated."""
        import threading
        import time
        
        results = []
        errors = []
        
        def run_interpreter(interpreter_id):
            try:
                executor = InterpretedLanguageExecutor(
                    interpreter_cmd=["python3"],
                    file_ext=".py"
                )
                
                temp_dir = executor.temp_dir
                
                with patch('subprocess.run') as mock_run:
                    mock_run.return_value = Mock(returncode=0, stdout=str(interpreter_id), stderr="")
                    
                    with patch.object(executor, 'prepare_code', return_value=f"print({interpreter_id})"):
                        test_case = {"parameters": {}, "expected": str(interpreter_id)}
                        result = executor.execute_test("test", test_case)
                        
                        # Simulate some work
                        time.sleep(0.1)
                        
                        results.append((interpreter_id, temp_dir, result["passed"]))
                
                executor.cleanup()
                
            except Exception as e:
                errors.append((interpreter_id, str(e)))
        
        # Run multiple interpreters concurrently
        threads = []
        for i in range(3):
            thread = threading.Thread(target=run_interpreter, args=(i,))
            threads.append(thread)
            thread.start()
        
        for thread in threads:
            thread.join()
        
        # All should succeed with different temp directories
        assert len(errors) == 0
        assert len(results) == 3
        
        temp_dirs = [temp_dir for _, temp_dir, _ in results]
        assert len(set(temp_dirs)) == 3  # All different temp directories
        
        # All temp directories should be cleaned up
        for temp_dir in temp_dirs:
            assert not os.path.exists(temp_dir)


class TestInterpretationErrorPropagation:
    """Test error propagation in interpretation workflows."""
    
    @patch('subprocess.run')
    def test_interpreter_not_found_error(self, mock_run):
        """Test error when interpreter is not found."""
        mock_run.side_effect = FileNotFoundError("No such file or directory: 'nonexistent_interpreter'")
        
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["nonexistent_interpreter"],
            file_ext=".xyz"
        )
        
        try:
            with patch.object(executor, 'prepare_code', return_value="test"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("test", test_case)
                
                assert result["passed"] is False
                assert "No such file or directory" in result["error"]
        
        finally:
            executor.cleanup()
    
    @patch('subprocess.run')
    def test_permission_error_propagation(self, mock_run):
        """Test error propagation for permission errors."""
        mock_run.side_effect = PermissionError("Permission denied")
        
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            with patch.object(executor, 'prepare_code', return_value="test"):
                test_case = {"parameters": {}, "expected": ""}
                result = executor.execute_test("test", test_case)
                
                assert result["passed"] is False
                assert "Permission denied" in result["error"]
        
        finally:
            executor.cleanup()
    
    @patch('subprocess.run')  
    def test_exception_handling_robustness(self, mock_run):
        """Test robustness of exception handling."""
        # Test various exception types
        exceptions_to_test = [
            OSError("System error"),
            RuntimeError("Runtime error"),
            ValueError("Value error"),
            Exception("Generic exception")
        ]
        
        executor = InterpretedLanguageExecutor(
            interpreter_cmd=["python3"],
            file_ext=".py"
        )
        
        try:
            for exception in exceptions_to_test:
                mock_run.side_effect = exception
                
                with patch.object(executor, 'prepare_code', return_value="test"):
                    test_case = {"parameters": {}, "expected": ""}
                    result = executor.execute_test("test", test_case)
                    
                    assert result["passed"] is False
                    assert str(exception) in result["error"]
                    assert result["actual"] is None
        
        finally:
            executor.cleanup()
