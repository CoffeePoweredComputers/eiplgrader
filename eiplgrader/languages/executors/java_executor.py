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
        # Use common validation
        self.validate_types_provided(test_case)
    
        function_name = test_case.get("function_name", "foo")
        params = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")
        param_types = test_case["parameter_types"]  # Required field
        expected_type = test_case["expected_type"]  # Required field
    
        # Extract the Solution class content
        # Check if code already has proper structure
        if "public class Test" in code:
            # Code already has Test class, likely from previous preparation
            return code
    
        import re
    
        # Extract import statements first
        import_pattern = r'^import\s+.*?;$'
        imports = re.findall(import_pattern, code, re.MULTILINE)
        
        # Remove imports from code
        code_without_imports = re.sub(import_pattern, '', code, flags=re.MULTILINE).strip()
    
        # Extract method from Solution class if present
        solution_match = re.search(
            r"public\s+class\s+Solution\s*\{(.*)\}(?:\s*$)", code_without_imports, re.DOTALL
        )
        if solution_match:
            method_code = solution_match.group(1).strip()
        else:
            # Assume the code is just the method
            method_code = code_without_imports.strip()
    
        # Build parameter declarations with embedded values
        param_names = list(params.keys())
        param_declarations = []
    
        for param_name in param_names:
            param_value = params[param_name]
            java_type = param_types[param_name]
            declaration = self._generate_param_declaration(
                param_name, java_type, param_value
            )
            param_declarations.append(declaration)
    
        # Build function call based on inplace mode
        if inplace_mode == "0":
            # Normal function call - returns a value
            function_call = f"Solution.{function_name}({', '.join(param_names)})"
            result_handling = self._generate_output(
                expected_type, function_call, is_direct_call=True
            )
        elif inplace_mode == "1":
            # In-place modification (for arrays/lists)
            if param_names:
                first_param = param_names[0]
                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )
                if other_params:
                    function_call = (
                        f"Solution.{function_name}({first_param}, {other_params})"
                    )
                else:
                    function_call = f"Solution.{function_name}({first_param})"
                result_handling = f"        {function_call};\n"
                first_type = param_types[first_param]
                result_handling += self._generate_output(first_type, first_param)
            else:
                function_call = f"Solution.{function_name}()"
                result_handling = (
                    f'        {function_call};\n        System.out.println("null");'
                )
        elif inplace_mode == "2":
            # Both modifies and returns
            if param_names:
                first_param = param_names[0]
                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )
                if other_params:
                    function_call = (
                        f"Solution.{function_name}({first_param}, {other_params})"
                    )
                else:
                    function_call = f"Solution.{function_name}({first_param})"
                result_handling = self._generate_output(
                    expected_type, function_call, is_direct_call=True
                )
            else:
                function_call = f"Solution.{function_name}()"
                result_handling = self._generate_output(
                    expected_type, function_call, is_direct_call=True
                )
        else:
            result_handling = (
                '        System.out.println("Error: Invalid inplace mode");'
            )
    
        # Build complete test harness with embedded values
        # Properly indent method code
        if method_code:
            # Add proper indentation if not already present
            indented_method = []
            for line in method_code.split("\n"):
                if line.strip():  # Non-empty line
                    if not line.startswith("    "):
                        indented_method.append("    " + line)
                    else:
                        indented_method.append(line)
                else:
                    indented_method.append(line)
            method_code = "\n".join(indented_method)
    
        # Build import section
        import_section = "import java.util.*;\n"
        if imports:
            import_section += "\n".join(imports) + "\n"
    
        test_harness = f"""{import_section}
    class Solution {{
    {method_code}
    }}
    
    public class Test {{
        public static void main(String[] args) {{
            // Test parameters
    {''.join(param_declarations)}
            
            // Call function and handle result
    {result_handling}
        }}
    }}"""
    
        return test_harness


    def _generate_param_declaration(self, name: str, java_type: str, value: Any) -> str:
        """Generate parameter declaration with embedded value."""
        if java_type == "int":
            return f"        int {name} = {value};\n"
        elif java_type == "double":
            return f"        double {name} = {value};\n"
        elif java_type == "boolean":
            return f"        boolean {name} = {str(value).lower()};\n"
        elif java_type == "String":
            return f'        String {name} = "{value}";\n'
        elif java_type == "int[]" and isinstance(value, list):
            values_str = ", ".join(str(v) for v in value)
            return f"        int[] {name} = new int[] {{{values_str}}};\n"
        elif java_type == "double[]" and isinstance(value, list):
            values_str = ", ".join(str(v) for v in value)
            return f"        double[] {name} = new double[] {{{values_str}}};\n"
        elif java_type == "String[]" and isinstance(value, list):
            values_str = ", ".join(f'"{v}"' for v in value)
            return f"        String[] {name} = new String[] {{{values_str}}};\n"
        else:
            return f"        // Unsupported type: {java_type} {name}\n"

    def _generate_output(
        self, java_type: str, expr: str, is_direct_call: bool = False
    ) -> str:
        """Generate output code for a Java expression."""
        if is_direct_call:
            # Wrap the expression in a variable assignment
            if java_type == "int":
                return f"        int result = {expr};\n        System.out.println(result);\n"
            elif java_type == "double":
                return f"        double result = {expr};\n        System.out.println(result);\n"
            elif java_type == "boolean":
                return f"        boolean result = {expr};\n        System.out.println(result);\n"
            elif java_type == "String":
                return f'        String result = {expr};\n        System.out.println("\\"" + result + "\\"");\n'
            elif java_type == "int[]":
                return f"""        int[] result = {expr};
        System.out.print("[");
        for (int i = 0; i < result.length; i++) {{
            if (i > 0) System.out.print(",");
            System.out.print(result[i]);
        }}
        System.out.println("]");
"""
            elif java_type == "double[]":
                return f"""        double[] result = {expr};
        System.out.print("[");
        for (int i = 0; i < result.length; i++) {{
            if (i > 0) System.out.print(",");
            System.out.print(result[i]);
        }}
        System.out.println("]");
"""
            elif java_type == "String[]":
                return f"""        String[] result = {expr};
        System.out.print("[");
        for (int i = 0; i < result.length; i++) {{
            if (i > 0) System.out.print(",");
            System.out.print("\\"" + result[i] + "\\"");
        }}
        System.out.println("]");
"""
            else:
                return f"        Object result = {expr};\n        System.out.println(result);\n"
        else:
            # Direct output of variable
            if java_type in ("int", "double", "boolean"):
                return f"        System.out.println({expr});\n"
            elif java_type == "String":
                return f'        System.out.println("\\"" + {expr} + "\\"");\n'
            elif java_type == "int[]":
                return f"""        System.out.print("[");
        for (int i = 0; i < {expr}.length; i++) {{
            if (i > 0) System.out.print(",");
            System.out.print({expr}[i]);
        }}
        System.out.println("]");
"""
            elif java_type == "double[]":
                return f"""        System.out.print("[");
        for (int i = 0; i < {expr}.length; i++) {{
            if (i > 0) System.out.print(",");
            System.out.print({expr}[i]);
        }}
        System.out.println("]");
"""
            elif java_type == "String[]":
                return f"""        System.out.print("[");
        for (int i = 0; i < {expr}.length; i++) {{
            if (i > 0) System.out.print(",");
            System.out.print("\\"" + {expr}[i] + "\\"");
        }}
        System.out.println("]");
"""
            else:
                return f"        System.out.println({expr});\n"

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
