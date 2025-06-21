"""Haskell language executor for code testing."""

import os
import json
import subprocess
from typing import Dict, Any, Tuple
from .base_executors import CompiledLanguageExecutor


class HaskellExecutor(CompiledLanguageExecutor):
    """Executor for Haskell language code testing."""

    def __init__(self):
        super().__init__(
            compile_cmd=["ghc"], run_cmd=None, file_ext=".hs", use_json_input=False
        )

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Haskell code for execution with test harness."""
        # Use common validation
        self.validate_types_provided(test_case)

        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        parameter_types = test_case[
            "parameter_types"
        ]  # Required field after validation
        expected_type = test_case["expected_type"]  # Required field after validation
        inplace_mode = test_case.get("inplace", "0")

        # Start building the module
        prepared_code = "module Main where\n\n"
        prepared_code += "import System.IO\n"
        prepared_code += "import Text.Read (readMaybe)\n\n"

        # Add the student's code
        prepared_code += code + "\n\n"

        # Generate main function with embedded values
        prepared_code += "main :: IO ()\n"
        prepared_code += "main = do\n"
        prepared_code += "    hSetBuffering stdout NoBuffering\n"
        prepared_code += "    -- Test parameters (embedded values)\n"

        # Generate parameter declarations with embedded values
        param_names = list(parameters.keys())

        for name, value in parameters.items():
            param_type = parameter_types[name]
            prepared_code += self._generate_param_declaration(name, param_type, value)

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
            prepared_code += '    putStrLn $ "\\"" ++ result ++ "\\""\n'
        elif expected_type and expected_type.startswith("["):
            prepared_code += "    print result\n"
        else:
            prepared_code += "    print result\n"

        return prepared_code

    def _generate_param_declaration(
        self, name: str, param_type: str, value: Any
    ) -> str:
        """Generate parameter declaration with embedded value."""
        if param_type == "Int":
            return f"    let {name} = {value} :: Int\n"
        elif param_type == "String":
            escaped_value = value.replace("\\", "\\\\").replace('"', '\\"')
            return f'    let {name} = "{escaped_value}" :: String\n'
        elif param_type == "Bool":
            haskell_bool = "True" if value else "False"
            return f"    let {name} = {haskell_bool} :: Bool\n"
        elif param_type == "Double":
            return f"    let {name} = {value} :: Double\n"
        elif param_type == "[Int]" and isinstance(value, list):
            return f"    let {name} = {value} :: [Int]\n"
        elif param_type == "[Double]" and isinstance(value, list):
            return f"    let {name} = {value} :: [Double]\n"
        elif param_type == "[String]" and isinstance(value, list):
            escaped_values = []
            for v in value:
                escaped = v.replace("\\", "\\\\").replace('"', '\\"')
                escaped_values.append(f'"{escaped}"')
            values_str = ", ".join(escaped_values)
            return f"    let {name} = [{values_str}] :: [String]\n"
        else:
            return f"    let {name} = {value} :: {param_type}\n"

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
