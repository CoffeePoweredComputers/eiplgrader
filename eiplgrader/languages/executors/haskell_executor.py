"""Haskell language executor for code testing."""

import json
import subprocess
from typing import Dict, Any, Tuple, Optional
from .base_executors import CompiledLanguageExecutor


class HaskellExecutor(CompiledLanguageExecutor):
    """Executor for Haskell language code testing."""

    def __init__(self):
        super().__init__(
            compile_cmd=["ghc"], run_cmd=None, file_ext=".hs", use_json_input=False
        )

    def extract_function_name(self, code: str) -> str:
        """Extract the main function name from Haskell code.

        Args:
            code: Haskell source code

        Returns:
            Function name if found, otherwise "main"
        """
        import re

        # Remove comments and strings to avoid false matches
        cleaned_code = re.sub(r"--.*$", "", code, flags=re.MULTILINE)
        cleaned_code = re.sub(r"{-.*?-}", "", cleaned_code, flags=re.DOTALL)
        cleaned_code = re.sub(r'"[^"]*"', '""', cleaned_code)

        # Look for function type signature: functionName :: Type
        # This is the most reliable way to identify function definitions
        type_sig_pattern = r"^(\w+)\s*::"
        matches = re.findall(type_sig_pattern, cleaned_code, re.MULTILINE)

        if matches:
            # Return the first function found (usually the main one)
            return matches[0]

        # Fallback: look for function definition pattern: functionName args =
        func_def_pattern = r"^(\w+)(?:\s+\w+)*\s*="
        matches = re.findall(func_def_pattern, cleaned_code, re.MULTILINE)

        if matches:
            # Filter out common Haskell keywords and built-ins
            keywords = {"main", "let", "where", "case", "if", "then", "else", "do"}
            valid_functions = [m for m in matches if m not in keywords]
            if valid_functions:
                return valid_functions[0]

        # If nothing found, return a default
        return "main"

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Haskell code for execution with test harness."""
        from .string_utils import CodeBuilder
        from .templates import (
            generate_haskell_param_declaration,
            generate_haskell_output,
        )

        # Use common validation
        self.validate_types_provided(test_case)

        # Try to get function name from test case, fallback to extraction
        function_name = test_case.get("function_name")
        if not function_name:
            function_name = self.extract_function_name(code)
        parameters = test_case.get("parameters", {})
        parameter_types = test_case[
            "parameter_types"
        ]  # Required field after validation
        expected_type = test_case["expected_type"]  # Required field after validation

        # Build the module using CodeBuilder
        builder = CodeBuilder()

        # Module header
        builder.add_line("module Main where")
        builder.add_line()

        # Standard imports
        builder.add_line("import System.IO")
        builder.add_line("import Text.Read (readMaybe)")

        # Check if code contains additional imports and add them
        code_lines = code.strip().split("\n")
        user_imports = []
        user_code_lines = []

        for line in code_lines:
            stripped_line = line.strip()
            if stripped_line.startswith("import "):
                # Extract user imports
                user_imports.append(line)
            elif stripped_line:  # Skip empty lines when building user code
                user_code_lines.append(line)

        # Add user imports if any
        if user_imports:
            for import_line in user_imports:
                builder.add_line(import_line)

        builder.add_line()

        # Add the student's code (excluding import lines)
        clean_code = "\n".join(user_code_lines)
        builder.add_lines(clean_code)
        builder.add_line()

        # Generate main function
        builder.add_line("main :: IO ()")
        builder.add_line("main = do")

        with builder.indent():
            builder.add_line("hSetBuffering stdout NoBuffering")
            builder.add_line("-- Test parameters (embedded values)")

            # Generate parameter declarations using template function
            param_names = list(parameters.keys())
            for name, value in parameters.items():
                param_type = parameter_types[name]
                param_decl = generate_haskell_param_declaration(name, param_type, value)
                builder.add_line(param_decl)

            # Add spacing if we have parameters
            if param_names:
                builder.add_line()

            # Generate function call
            if param_names:
                builder.add_line(
                    f"let result = {function_name} {' '.join(param_names)}"
                )
            else:
                builder.add_line(f"let result = {function_name}")

            # Generate output using template function
            output_code = generate_haskell_output(expected_type, "result")
            builder.add_line(output_code)

        return builder.build()

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile Haskell code, return (success, output_path, error)"""
        output_path = code_path.replace(self.file_ext, "")
        cmd = self.compile_cmd + ["-O0", "-o", output_path, code_path]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return True, output_path, ""
        else:
            return False, output_path, result.stderr.strip()

    def normalize_output(
        self, raw_output: str, expected_type: Optional[str] = None
    ) -> Any:
        """Handle Haskell-specific output parsing."""
        output = raw_output.strip()

        # Handle boolean output
        if output == "true":
            return True
        elif output == "false":
            return False

        # Handle tuple output like "(3,2)" -> [3, 2]
        if (
            expected_type
            and expected_type.startswith("(")
            and expected_type.endswith(")")
            and "," in expected_type
        ):
            # Haskell tuple format: (value1,value2,...)
            if output.startswith("(") and output.endswith(")"):
                # Remove parentheses and split by comma
                inner = output[1:-1]
                parts: list[Any] = []
                # Simple split by comma (works for basic types)
                for part in inner.split(","):
                    part = part.strip()
                    # Try to parse each part
                    if part == "True":
                        parts.append(True)
                    elif part == "False":
                        parts.append(False)
                    elif part.startswith('"') and part.endswith('"'):
                        parts.append(part[1:-1])  # Remove quotes
                    else:
                        try:
                            # Try integer first, then float
                            if "." in part:
                                parts.append(float(part))
                            else:
                                parts.append(int(part))
                        except ValueError:
                            parts.append(part)  # Keep as string
                return parts

        # Handle string output (remove quotes)
        if output.startswith('"') and output.endswith('"'):
            return output[1:-1]

        # Try to parse as JSON
        try:
            return json.loads(output)
        except json.JSONDecodeError:
            # If JSON parsing fails, return the raw output
            return output
