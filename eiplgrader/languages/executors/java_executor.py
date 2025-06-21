"""Java language executor for code testing."""

import json
import os
import subprocess
import tempfile
from typing import Dict, Any, Tuple
from ..executors.base_executors import CompiledLanguageExecutor


class JavaExecutor(CompiledLanguageExecutor):
    """Executor for Java language code testing."""

    def __init__(self):
        super().__init__(
            compile_cmd=["javac"],
            run_cmd=["java"],
            file_ext=".java",
            use_json_input=False,
        )

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Java code for execution with test harness."""
        from .string_utils import CodeBuilder
        from .templates import generate_java_param_declaration, generate_java_output
        
        # Use common validation
        self.validate_types_provided(test_case)

        function_name = test_case.get("function_name", "foo")
        params = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")
        param_types = test_case["parameter_types"]  # Required field
        expected_type = test_case["expected_type"]  # Required field

        # Check if code already has proper structure
        if "public class Test" in code:
            return code

        # Initialize code builder
        builder = CodeBuilder()
        
        # Extract and build imports
        import re
        import_pattern = r"^import\s+.*?;$"
        imports = re.findall(import_pattern, code, re.MULTILINE)
        
        builder.add_line("import java.util.*;")
        for import_stmt in imports:
            builder.add_line(import_stmt)
        builder.add_line()

        # Extract method code
        code_without_imports = re.sub(
            import_pattern, "", code, flags=re.MULTILINE
        ).strip()

        solution_match = re.search(
            r"public\s+class\s+Solution\s*\{(.*)\}(?:\s*$)",
            code_without_imports,
            re.DOTALL,
        )
        if solution_match:
            method_code = solution_match.group(1).strip()
        else:
            method_code = code_without_imports.strip()

        # Build Solution class
        builder.add_line("class Solution {")
        with builder.indent():
            if method_code:
                # Clean and add method code with proper indentation
                self._add_method_code(builder, method_code)
        builder.add_line("}")
        builder.add_line()

        # Build Test class
        builder.add_line("public class Test {")
        with builder.indent():
            builder.add_line("public static void main(String[] args) {")
            with builder.indent():
                # Generate parameter declarations
                builder.add_line("// Test parameters")
                param_names = list(params.keys())
                for param_name in param_names:
                    param_value = params[param_name]
                    java_type = param_types[param_name]
                    declaration = generate_java_param_declaration(
                        param_name, java_type, param_value
                    )
                    builder.add_lines(declaration)
                
                builder.add_line()
                builder.add_line("// Call function and handle result")
                
                # Generate function call and result handling
                result_code = self._generate_result_handling(
                    function_name, param_names, param_types, expected_type, inplace_mode
                )
                builder.add_lines(result_code)
                
            builder.add_line("}")
        builder.add_line("}")

        return builder.build()

    def _add_method_code(self, builder: "CodeBuilder", method_code: str) -> None:
        """Add method code with proper indentation."""
        lines = method_code.split("\n")
        for line in lines:
            builder.add_line(line.strip() if line.strip() else "")

    def _generate_result_handling(
        self, 
        function_name: str, 
        param_names: list, 
        param_types: dict, 
        expected_type: str, 
        inplace_mode: str
    ) -> str:
        """Generate result handling code based on inplace mode."""
        from .string_utils import CodeBuilder
        from .templates import generate_java_output
        
        builder = CodeBuilder()
        
        if inplace_mode == "0":
            # Normal function call - returns a value
            function_call = f"Solution.{function_name}({', '.join(param_names)})"
            if expected_type in ["int", "double", "boolean", "String"]:
                builder.add_line(f"{expected_type} result = {function_call};")
                output_code = generate_java_output(expected_type, "result")
            else:
                builder.add_line(f"var result = {function_call};")
                output_code = generate_java_output(expected_type, "result")
            builder.add_lines(output_code)
            
        elif inplace_mode == "1":
            # In-place modification (for arrays/lists)
            if param_names:
                first_param = param_names[0]
                other_params = ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                if other_params:
                    function_call = f"Solution.{function_name}({first_param}, {other_params})"
                else:
                    function_call = f"Solution.{function_name}({first_param})"
                builder.add_line(f"{function_call};")
                first_type = param_types[first_param]
                output_code = generate_java_output(first_type, first_param)
                builder.add_lines(output_code)
            else:
                function_call = f"Solution.{function_name}()"
                builder.add_line(f'{function_call};')
                builder.add_line('System.out.println("null");')
                
        elif inplace_mode == "2":
            # Both modifies and returns
            if param_names:
                first_param = param_names[0]
                other_params = ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                if other_params:
                    function_call = f"Solution.{function_name}({first_param}, {other_params})"
                else:
                    function_call = f"Solution.{function_name}({first_param})"
                if expected_type in ["int", "double", "boolean", "String"]:
                    builder.add_line(f"{expected_type} result = {function_call};")
                    output_code = generate_java_output(expected_type, "result")
                else:
                    builder.add_line(f"var result = {function_call};")
                    output_code = generate_java_output(expected_type, "result")
                builder.add_lines(output_code)
            else:
                function_call = f"Solution.{function_name}()"
                if expected_type in ["int", "double", "boolean", "String"]:
                    builder.add_line(f"{expected_type} result = {function_call};")
                    output_code = generate_java_output(expected_type, "result")
                else:
                    builder.add_line(f"var result = {function_call};")
                    output_code = generate_java_output(expected_type, "result")
                builder.add_lines(output_code)
        else:
            builder.add_line('System.out.println("Error: Invalid inplace mode");')

        return builder.build()

    def _generate_param_declaration(self, name: str, java_type: str, value: Any) -> str:
        """Generate parameter declaration with embedded value."""
        from .templates import generate_java_param_declaration
        return generate_java_param_declaration(name, java_type, value)

    def _generate_output(
        self, java_type: str, expr: str, is_direct_call: bool = False
    ) -> str:
        """Generate output code for a Java expression."""
        from .templates import generate_java_output
        return generate_java_output(java_type, expr, is_direct_call)

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile Java code."""
        output_dir = os.path.dirname(code_path)
        cmd = ["javac", "-d", output_dir, code_path]
        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return True, output_dir, ""
        else:
            return False, "", result.stderr

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Override to not send JSON input since we embed values directly."""
        # Prepare code with test harness
        prepared_code = self.prepare_code(code, test_case)

        # Write to temporary file
        code_path = os.path.join(self.temp_dir, "Test.java")
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

        # Execute without input since values are embedded
        try:
            cmd = ["java", "-cp", output_path, "Test"]
            result = subprocess.run(
                cmd,
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