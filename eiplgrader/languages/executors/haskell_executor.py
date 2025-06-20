"""Haskell language executor for code testing."""

import os
import json
import subprocess
from typing import Dict, Any, Tuple
from .base_executors import CompiledLanguageExecutor


class HaskellExecutor(CompiledLanguageExecutor):
    """Executor for Haskell language code testing."""

    def __init__(self):
        super().__init__(compile_cmd=["ghc"], run_cmd=None, file_ext=".hs")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Haskell code for execution with test harness."""
        # Validate required type information using standardized error message
        errors = []
        if "parameter_types" not in test_case:
            errors.append("parameter_types not provided")
        if "expected_type" not in test_case:
            errors.append("expected_type not provided")

        if errors:
            error_msg = "Missing required type information:\n"
            for error in errors:
                error_msg += f"- {error}\n"
            error_msg += "\nTest case must include:\n"
            error_msg += "{\n"
            error_msg += '    "parameters": {...},\n'
            error_msg += '    "parameter_types": {"param1": "type1", ...},\n'
            error_msg += '    "expected": ...,\n'
            error_msg += '    "expected_type": "type"\n'
            error_msg += "}"
            raise ValueError(error_msg)

        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        parameter_types = test_case.get("parameter_types", {})
        expected_type = test_case.get("expected_type")
        inplace_mode = test_case.get("inplace", "0")

        # Validate all parameters have types
        for param_name in parameters:
            if param_name not in parameter_types:
                raise ValueError(
                    f"Missing required type information:\n- parameter_types['{param_name}'] not provided"
                )

        # Start building the module
        prepared_code = "module Main where\n\n"
        prepared_code += "import System.IO\n"
        prepared_code += "import Text.Read (readMaybe)\n\n"

        # Add the student's code
        prepared_code += code + "\n\n"

        # Generate main function
        prepared_code += "main :: IO ()\n"
        prepared_code += "main = do\n"
        prepared_code += "    hSetBuffering stdout NoBuffering\n"
        prepared_code += "    inputStr <- getLine\n"

        # Parse parameters using explicit types
        param_names = list(parameters.keys())

        # Generate parameter parsing based on explicit types
        for name, value in parameters.items():
            param_type = parameter_types[name]

            # Parse based on type declaration
            if param_type == "Int":
                prepared_code += f"    let {name} = read (takeWhile (/= ',') $ dropWhile (/= ':') $ dropWhile (/= '\"') inputStr) :: Int\n"
            elif param_type == "String":
                prepared_code += f'    let {name} = "{value}"\n'
            elif param_type == "Bool":
                prepared_code += f"    let {name} = {str(value).lower()}\n"
            elif param_type == "Double":
                prepared_code += f"    let {name} = {value} :: Double\n"
            elif param_type == "[Int]":
                prepared_code += f"    let {name} = {value} :: [Int]\n"
            else:
                # For complex types, use the literal value
                prepared_code += f"    let {name} = {value} :: {param_type}\n"

        # Generate function call
        if param_names:
            prepared_code += (
                f"    let result = {function_name} {' '.join(param_names)}\n"
            )
        else:
            prepared_code += f"    let result = {function_name}\n"

        # Output based on explicit expected type
        if expected_type == "Bool":
            prepared_code += '    putStrLn $ if result then "true" else "false"\n'
        elif expected_type in ["Int", "Double"]:
            prepared_code += "    print result\n"
        elif expected_type == "String":
            prepared_code += '    putStrLn $ "\\"" ++ result ++ "\\""\\n'
        elif expected_type.startswith("["):
            prepared_code += "    print result\n"
        else:
            prepared_code += "    print result\n"

        return prepared_code

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile Haskell code, return (success, output_path, error)"""
        output_path = code_path.replace(self.file_ext, "")
        cmd = self.compile_cmd + ["-O0", "-o", output_path, code_path]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return True, output_path, ""
        else:
            return False, output_path, result.stderr.strip()

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute Haskell test."""
        try:
            result = super().execute_test(code, test_case)

            # Clean up output if needed
            if result.get("output"):
                output = result["output"].strip()

                # Handle boolean output
                if output == "true":
                    result["actual"] = True
                elif output == "false":
                    result["actual"] = False
                # Handle string output (remove quotes)
                elif output.startswith('"') and output.endswith('"'):
                    result["actual"] = output[1:-1]
                else:
                    # Try to parse as JSON
                    try:
                        result["actual"] = json.loads(output)
                    except:
                        result["actual"] = output

                # Re-check if test passed
                result["passed"] = result["actual"] == test_case.get("expected")

            return result

        except Exception as e:
            return {
                "passed": False,
                "error": str(e),
                "actual": None,
                "expected": test_case.get("expected"),
            }
