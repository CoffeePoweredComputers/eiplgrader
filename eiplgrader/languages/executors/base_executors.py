"""Base executors for different language types."""

import subprocess
import tempfile
import json
import os
import shutil
from typing import Dict, Any, List, Optional, Tuple
from abc import ABC, abstractmethod


class LanguageExecutor(ABC):
    """Abstract base class for all language executors."""

    @abstractmethod
    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare code for execution with test harness."""
        pass

    @abstractmethod
    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute code with test case and return results."""
        pass

    @abstractmethod
    def cleanup(self) -> None:
        """Clean up any temporary resources."""
        pass

    def validate_types_provided(self, test_case: Dict[str, Any]) -> None:
        """Validate that required type information is provided for non-JSON languages."""
        errors = []
        if "parameter_types" not in test_case:
            errors.append("parameter_types not provided")
        if "expected_type" not in test_case:
            errors.append("expected_type not provided")

        if errors:
            error_msg = "Missing required type information:\n"
            error_msg += "\n".join(f"- {error}" for error in errors) + "\n"
            error_msg += "\nTest case must include:\n"
            error_msg += "{\n"
            error_msg += '    "parameters": {...},\n'
            error_msg += '    "parameter_types": {"param1": "type1", ...},\n'
            error_msg += '    "expected": ...,\n'
            error_msg += '    "expected_type": "type"\n'
            error_msg += "}"
            raise ValueError(error_msg)

        # Validate all parameters have types
        parameters = test_case.get("parameters", {})
        parameter_types = test_case.get("parameter_types", {})
        for param_name in parameters:
            if param_name not in parameter_types:
                raise ValueError(
                    f"Missing required type information:\n- parameter_types['{param_name}'] not provided"
                )

    def infer_type(self, value: Any) -> str:
        """Infer type from a Python value. Used by JSON-capable languages."""
        if isinstance(value, bool):
            return "bool"
        elif isinstance(value, int):
            return "int"
        elif isinstance(value, float):
            return "double"
        elif isinstance(value, str):
            return "string"
        elif isinstance(value, list):
            if value:
                # Check first element, but bool must be checked after int
                if isinstance(value[0], bool):
                    # In Python, bool is subclass of int, so treat as generic
                    return "List"
                elif isinstance(value[0], int):
                    return "List[int]"
                elif isinstance(value[0], float):
                    return "List[double]"
                elif isinstance(value[0], str):
                    return "List[string]"
            return "List"
        return "unknown"
    
    def normalize_output(self, raw_output: str, expected_type: str = None) -> Any:
        """Override in subclasses for language-specific output normalization."""
        output = raw_output.strip()
        
        # Default: try JSON, fallback to raw
        if output:
            try:
                return json.loads(output)
            except json.JSONDecodeError:
                return output
        else:
            return ""
    
    def enhance_error_message(self, error_msg: str, stderr: str = "") -> str:
        """Override in subclasses for language-specific error message enhancement."""
        return error_msg
    
    def generate_function_call_repr(self, test_case: Dict[str, Any]) -> str:
        """Generate standardized function call representation."""
        function_name = test_case.get("function_name", "foo")
        params = test_case.get("parameters", {})
        args = list(params.values())
        return f"{function_name}({', '.join(map(repr, args))})"

class CompiledLanguageExecutor(LanguageExecutor):
    """Base executor for compiled languages (C, C++, Java, Go, Haskell)."""

    def __init__(
        self,
        compile_cmd: List[str],
        run_cmd: Optional[List[str]],
        file_ext: str,
        use_json_input: bool = False,
    ):
        self.compile_cmd = compile_cmd
        self.run_cmd = run_cmd
        self.file_ext = file_ext
        self.use_json_input = use_json_input
        self.temp_dir = tempfile.mkdtemp()

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile code, return (success, output_path, error)"""
        output_path = code_path.replace(self.file_ext, "")
        cmd = self.compile_cmd + ["-o", output_path, code_path]

        result = subprocess.run(cmd, capture_output=True, text=True)
        return (result.returncode == 0, output_path, result.stderr)

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Compile and execute test"""
        # Prepare code with test harness
        prepared_code = self.prepare_code(code, test_case)

        # Write to temporary file
        code_path = os.path.join(self.temp_dir, f"test{self.file_ext}")
        with open(code_path, "w") as f:
            f.write(prepared_code)

        # Compile
        success, output_path, error = self.compile(code_path)
        if not success:
            enhanced_error = self.enhance_error_message(f"Compilation failed: {error}", error)
            return {
                "passed": False,
                "error": enhanced_error,
                "actual": None,
                "expected": test_case.get("expected"),
            }

        # Execute
        try:
            if self.use_json_input:
                # Pass test parameters as JSON stdin
                args_json = json.dumps(test_case.get("parameters", {}))
                result = subprocess.run(
                    [output_path],
                    input=args_json,
                    capture_output=True,
                    text=True,
                    timeout=test_case.get("timeout", 30),
                )
            else:
                # No input needed for embedded values
                result = subprocess.run(
                    [output_path],
                    capture_output=True,
                    text=True,
                    timeout=test_case.get("timeout", 30),
                )

            if result.returncode != 0:
                enhanced_error = self.enhance_error_message(f"Runtime error: {result.stderr}", result.stderr)
                return {
                    "passed": False,
                    "error": enhanced_error,
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            # Use normalize_output hook for language-specific output parsing
            actual = self.normalize_output(result.stdout, test_case.get("expected_type"))
            passed = actual == test_case.get("expected")

            return {
                "passed": passed,
                "actual": actual,
                "expected": test_case.get("expected"),
                "output": result.stdout,
                "function_call": self.generate_function_call_repr(test_case),
            }

        except subprocess.TimeoutExpired:
            return {
                "passed": False,
                "error": "Execution timeout",
                "actual": None,
                "expected": test_case.get("expected"),
            }
        except Exception as e:
            enhanced_error = self.enhance_error_message(str(e))
            return {
                "passed": False,
                "error": enhanced_error,
                "actual": None,
                "expected": test_case.get("expected"),
            }


    def cleanup(self) -> None:
        """Clean up temporary directory"""
        if (
            hasattr(self, "temp_dir")
            and self.temp_dir
            and os.path.exists(self.temp_dir)
        ):
            try:
                shutil.rmtree(self.temp_dir)
            except (OSError, PermissionError):
                pass  # Directory already removed or inaccessible


class InterpretedLanguageExecutor(LanguageExecutor):
    """Base executor for interpreted languages (Python, JavaScript)."""

    def __init__(self, interpreter_cmd: List[str], file_ext: str):
        self.interpreter_cmd = interpreter_cmd
        self.file_ext = file_ext
        self.temp_dir = tempfile.mkdtemp()

    def validate_or_infer_types(self, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Validate types if provided, or infer them from values."""
        # For interpreted languages, types are optional
        if "parameter_types" not in test_case:
            test_case["parameter_types"] = {}

        # Infer types for any missing parameter types
        for param_name, value in test_case.get("parameters", {}).items():
            if param_name not in test_case["parameter_types"]:
                test_case["parameter_types"][param_name] = self.infer_type(value)

        if "expected_type" not in test_case:
            test_case["expected_type"] = self.infer_type(test_case.get("expected"))

        return test_case

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute test with interpreter"""
        # Prepare code with test harness
        prepared_code = self.prepare_code(code, test_case)

        # Write to temporary file
        code_path = os.path.join(self.temp_dir, f"test{self.file_ext}")
        with open(code_path, "w") as f:
            f.write(prepared_code)

        # Execute
        try:
            result = subprocess.run(
                self.interpreter_cmd + [code_path],
                capture_output=True,
                text=True,
                timeout=test_case.get("timeout", 30),
            )

            if result.returncode != 0:
                enhanced_error = self.enhance_error_message(f"Runtime error: {result.stderr}", result.stderr)
                return {
                    "passed": False,
                    "error": enhanced_error,
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            # Use normalize_output hook for language-specific output parsing
            actual = self.normalize_output(result.stdout, test_case.get("expected_type"))
            passed = actual == test_case.get("expected")

            return {
                "passed": passed,
                "actual": actual,
                "expected": test_case.get("expected"),
                "output": result.stdout,
                "function_call": self.generate_function_call_repr(test_case),
            }

        except subprocess.TimeoutExpired:
            return {
                "passed": False,
                "error": "Execution timeout",
                "actual": None,
                "expected": test_case.get("expected"),
            }
        except Exception as e:
            enhanced_error = self.enhance_error_message(str(e))
            return {
                "passed": False,
                "error": enhanced_error,
                "actual": None,
                "expected": test_case.get("expected"),
            }


    def cleanup(self) -> None:
        """Clean up temporary directory"""
        if (
            hasattr(self, "temp_dir")
            and self.temp_dir
            and os.path.exists(self.temp_dir)
        ):
            try:
                shutil.rmtree(self.temp_dir)
            except (OSError, PermissionError):
                pass  # Directory already removed or inaccessible
