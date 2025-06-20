"""Shared base executors for different language types."""

import subprocess
import tempfile
import json
import os
import shutil
from typing import Dict, Any, List, Optional, Tuple
from ..base import LanguageExecutor


class CompiledLanguageExecutor(LanguageExecutor):
    """Base executor for compiled languages"""

    def __init__(self, compile_cmd: List[str], run_cmd: List[str], file_ext: str, use_json_input: bool = True):
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
            return {
                "passed": False,
                "error": f"Compilation failed: {error}",
                "actual": None,
                "expected": test_case.get("expected"),
            }

        # Execute
        try:
            # Pass test parameters as stdin if needed
            if self.use_json_input:
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
                return {
                    "passed": False,
                    "error": f"Runtime error: {result.stderr}",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            # Parse output
            try:
                actual = json.loads(result.stdout.strip())
            except json.JSONDecodeError:
                actual = result.stdout.strip()

            passed = actual == test_case.get("expected")

            return {
                "passed": passed,
                "actual": actual,
                "expected": test_case.get("expected"),
                "output": result.stdout,
            }

        except subprocess.TimeoutExpired:
            return {
                "passed": False,
                "error": "Execution timeout",
                "actual": None,
                "expected": test_case.get("expected"),
            }
        except Exception as e:
            return {
                "passed": False,
                "error": str(e),
                "actual": None,
                "expected": test_case.get("expected"),
            }

    def cleanup(self) -> None:
        """Clean up temporary directory"""
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)


class InterpretedLanguageExecutor(LanguageExecutor):
    """Base executor for interpreted languages"""

    def __init__(self, interpreter_cmd: List[str], file_ext: str):
        self.interpreter_cmd = interpreter_cmd
        self.file_ext = file_ext
        self.temp_dir = tempfile.mkdtemp()

    def infer_type(self, value: Any) -> str:
        """Infer type from a Python value."""
        if isinstance(value, bool):
            return "bool"
        elif isinstance(value, int):
            return "int"
        elif isinstance(value, float):
            return "double"
        elif isinstance(value, str):
            return "string"
        elif isinstance(value, list):
            if value and isinstance(value[0], int):
                return "List[int]"
            elif value and isinstance(value[0], float):
                return "List[double]"
            elif value and isinstance(value[0], str):
                return "List[string]"
            return "List"
        return "unknown"

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute test directly"""
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
                return {
                    "passed": False,
                    "error": f"Runtime error: {result.stderr}",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            # Parse output
            try:
                actual = json.loads(result.stdout.strip())
            except json.JSONDecodeError:
                actual = result.stdout.strip()

            passed = actual == test_case.get("expected")

            return {
                "passed": passed,
                "actual": actual,
                "expected": test_case.get("expected"),
                "output": result.stdout,
            }

        except subprocess.TimeoutExpired:
            return {
                "passed": False,
                "error": "Execution timeout",
                "actual": None,
                "expected": test_case.get("expected"),
            }
        except Exception as e:
            return {
                "passed": False,
                "error": str(e),
                "actual": None,
                "expected": test_case.get("expected"),
            }

    def cleanup(self) -> None:
        """Clean up temporary directory"""
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)
