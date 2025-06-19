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
        super().__init__(compile_cmd=["javac"], run_cmd=["java"], file_ext=".java")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Java code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        params = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")
        param_types = test_case.get("param_types", {})

        # Extract the Solution class content
        # Check if code already has proper structure
        if "public class Test" in code:
            # Code already has Test class, likely from previous preparation
            return code

        # Extract method from Solution class if present
        import re

        solution_match = re.search(
            r"public\s+class\s+Solution\s*\{(.*?)\}", code, re.DOTALL
        )
        if solution_match:
            method_code = solution_match.group(1).strip()
        else:
            # Assume the code is just the method
            method_code = code.strip()

        # Build parameter parsing code
        param_names = list(params.keys())
        param_declarations = []
        param_list = []

        for i, param_name in enumerate(param_names):
            param_value = params[param_name]
            # Check if type hint provided, otherwise infer
            if param_name in param_types:
                java_type = param_types[param_name]
            else:
                java_type = self._infer_java_type(param_value, param_name)
            parse_code = self._get_parse_code(
                param_name, f"args[{i}]", java_type, param_value
            )
            param_declarations.append(f"        {parse_code}")
            param_list.append(param_name)

        # Build function call based on inplace mode
        if inplace_mode == "0":
            # Normal function call - returns a value
            function_call = f"Solution.{function_name}({', '.join(param_list)})"
            result_handling = f"""        Object result = {function_call};
        System.out.println(toJson(result));"""
        elif inplace_mode == "1":
            # In-place modification (for arrays/lists)
            if param_list:
                first_param = param_list[0]
                other_params = ", ".join(param_list[1:]) if len(param_list) > 1 else ""
                if other_params:
                    function_call = (
                        f"Solution.{function_name}({first_param}, {other_params})"
                    )
                else:
                    function_call = f"Solution.{function_name}({first_param})"
                result_handling = f"""        {function_call};
        System.out.println(toJson({first_param}));"""
            else:
                function_call = f"Solution.{function_name}()"
                result_handling = f"""        Object result = {function_call};
        System.out.println(toJson(result));"""
        elif inplace_mode == "2":
            # Both modifies and returns
            if param_list:
                first_param = param_list[0]
                other_params = ", ".join(param_list[1:]) if len(param_list) > 1 else ""
                if other_params:
                    function_call = (
                        f"Solution.{function_name}({first_param}, {other_params})"
                    )
                else:
                    function_call = f"Solution.{function_name}({first_param})"
                result_handling = f"""        Object result = {function_call};
        System.out.println(toJson(result != null ? result : {first_param}));"""
            else:
                function_call = f"Solution.{function_name}()"
                result_handling = f"""        Object result = {function_call};
        System.out.println(toJson(result));"""
        else:
            result_handling = (
                '        System.out.println("Error: Invalid inplace mode");'
            )

        # Build complete test harness
        test_harness = f"""import java.util.*;
import com.google.gson.Gson;

class Solution {{
    {method_code}
}}

public class Test {{
    private static String toJson(Object obj) {{
        return new Gson().toJson(obj);
    }}
    
    public static void main(String[] args) {{
        // Parse command line arguments
{chr(10).join(param_declarations)}
        
        // Call function and handle result
{result_handling}
    }}
}}"""

        return test_harness

    def _infer_java_type(self, value: Any, param_name: str = None) -> str:
        """Infer Java type from Python value."""
        if isinstance(value, bool):
            return "boolean"
        elif isinstance(value, int):
            return "int"
        elif isinstance(value, float):
            return "double"
        elif isinstance(value, str):
            return "String"
        elif isinstance(value, list):
            if value and isinstance(value[0], int):
                return "int[]"
            elif value and isinstance(value[0], float):
                return "double[]"
            elif value and isinstance(value[0], str):
                return "String[]"
            elif not value:
                # Empty array - try to infer from parameter name
                if param_name:
                    param_lower = param_name.lower()
                    if any(hint in param_lower for hint in ['string', 'str', 'text', 'word']):
                        return "String[]"
                    elif any(hint in param_lower for hint in ['double', 'float', 'decimal']):
                        return "double[]"
                    elif any(hint in param_lower for hint in ['bool', 'boolean', 'flag']):
                        return "boolean[]"
                # Default to int[] for numeric contexts
                return "int[]"
            else:
                return "Object[]"
        else:
            return "Object"

    def _get_parse_code(
        self, var_name: str, arg_expr: str, java_type: str, _value: Any
    ) -> str:
        """Generate code to parse command line argument into Java type."""
        if java_type == "int":
            return f"int {var_name} = Integer.parseInt({arg_expr});"
        elif java_type == "double":
            return f"double {var_name} = Double.parseDouble({arg_expr});"
        elif java_type == "boolean":
            return f"boolean {var_name} = Boolean.parseBoolean({arg_expr});"
        elif java_type == "String":
            return f"String {var_name} = {arg_expr};"
        elif java_type == "int[]":
            return f"""String {var_name}_str = {arg_expr};
        int[] {var_name};
        if ({var_name}_str.isEmpty()) {{
            {var_name} = new int[0];
        }} else {{
            String[] {var_name}_parts = {var_name}_str.split(",");
            {var_name} = new int[{var_name}_parts.length];
            for (int i = 0; i < {var_name}_parts.length; i++) {{
                {var_name}[i] = Integer.parseInt({var_name}_parts[i].trim());
            }}
        }}"""
        elif java_type == "double[]":
            return f"""String {var_name}_str = {arg_expr};
        double[] {var_name};
        if ({var_name}_str.isEmpty()) {{
            {var_name} = new double[0];
        }} else {{
            String[] {var_name}_parts = {var_name}_str.split(",");
            {var_name} = new double[{var_name}_parts.length];
            for (int i = 0; i < {var_name}_parts.length; i++) {{
                {var_name}[i] = Double.parseDouble({var_name}_parts[i].trim());
            }}
        }}"""
        elif java_type == "String[]":
            return f"""String {var_name}_str = {arg_expr};
        String[] {var_name};
        if ({var_name}_str.isEmpty()) {{
            {var_name} = new String[0];
        }} else {{
            {var_name} = {var_name}_str.split(",");
            for (int i = 0; i < {var_name}.length; i++) {{
                {var_name}[i] = {var_name}[i].trim();
            }}
        }}"""
        else:
            return f"Object {var_name} = {arg_expr};"

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile Java code with classpath for Gson."""
        output_dir = os.path.dirname(code_path)

        # Try to find Gson in common locations
        gson_paths = [
            "/usr/share/java/gson.jar",
            "/usr/local/share/java/gson.jar",
            os.path.expanduser(
                "~/.m2/repository/com/google/code/gson/gson/2.10.1/gson-2.10.1.jar"
            ),
            os.path.expanduser(
                "~/.gradle/caches/modules-2/files-2.1/com.google.code.gson/gson/"
            ),
        ]

        gson_jar = None
        for path in gson_paths:
            if os.path.exists(path):
                if os.path.isfile(path):
                    gson_jar = path
                    break
                elif os.path.isdir(path):
                    # Search for any gson jar in gradle cache
                    for root, dirs, files in os.walk(path):
                        for file in files:
                            if file.startswith("gson") and file.endswith(".jar"):
                                gson_jar = os.path.join(root, file)
                                break
                        if gson_jar:
                            break

        # Compile with or without Gson
        if gson_jar:
            cmd = ["javac", "-cp", f".:{gson_jar}", "-d", output_dir, code_path]
        else:
            # Fallback: compile without Gson
            cmd = ["javac", "-d", output_dir, code_path]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return True, output_dir, ""
        else:
            # If Gson compilation fails, try without it
            if gson_jar and "gson" in result.stderr.lower():
                cmd = ["javac", "-d", output_dir, code_path]
                result = subprocess.run(cmd, capture_output=True, text=True)
                if result.returncode == 0:
                    return True, output_dir, ""

            return False, "", result.stderr

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Compile and execute Java test."""
        # Prepare code with test harness
        prepared_code = self.prepare_code(code, test_case)
        
        # Check if we need to use a simpler version without Gson
        if "import com.google.gson.Gson" in prepared_code:
            # Try with Gson first
            result = self._execute_with_gson(prepared_code, test_case)
            if (
                result.get("error", "").startswith("Compilation failed")
                and "gson" in result.get("error", "").lower()
            ):
                # Fallback to simple JSON
                prepared_code = self._convert_to_simple_json(prepared_code)
                return self._execute_with_simple_json(prepared_code, test_case)
            return result
        else:
            return self._execute_with_simple_json(prepared_code, test_case)

    def _execute_with_gson(
        self, code: str, test_case: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Try to execute with Gson support."""
        # Write to temporary file
        code_path = os.path.join(self.temp_dir, "Test.java")
        with open(code_path, "w") as f:
            f.write(code)

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
            # Build command line arguments
            params = test_case.get("parameters", {})
            args = []
            for param_name, param_value in params.items():
                if isinstance(param_value, list):
                    args.append(",".join(map(str, param_value)))
                else:
                    args.append(str(param_value))

            # Find Gson jar for runtime
            gson_jar = None
            gson_paths = [
                "/usr/share/java/gson.jar",
                "/usr/local/share/java/gson.jar",
                os.path.expanduser(
                    "~/.m2/repository/com/google/code/gson/gson/2.10.1/gson-2.10.1.jar"
                ),
            ]
            for path in gson_paths:
                if os.path.exists(path):
                    gson_jar = path
                    break

            if gson_jar:
                cmd = ["java", "-cp", f"{output_path}:{gson_jar}", "Test"] + args
            else:
                cmd = ["java", "-cp", output_path, "Test"] + args

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

    def _execute_with_simple_json(
        self, code: str, test_case: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Execute with simple JSON implementation."""
        # Write to temporary file
        code_path = os.path.join(self.temp_dir, "Test.java")
        with open(code_path, "w") as f:
            f.write(code)

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
            # Build command line arguments
            params = test_case.get("parameters", {})
            args = []
            for param_name, param_value in params.items():
                if isinstance(param_value, list):
                    args.append(",".join(map(str, param_value)))
                else:
                    args.append(str(param_value))

            cmd = ["java", "-cp", output_path, "Test"] + args

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

    def _convert_to_simple_json(self, code: str) -> str:
        """Convert code to use simple JSON serialization instead of Gson."""
        # Remove Gson import
        code = code.replace("import com.google.gson.Gson;", "")

        # Replace Gson toJson with simple implementation
        simple_json = r"""    private static String toJson(Object obj) {
        if (obj == null) return "null";
        if (obj instanceof Boolean || obj instanceof Number) return obj.toString();
        if (obj instanceof String) return "\"" + obj + "\"";
        if (obj instanceof int[]) {
            int[] arr = (int[]) obj;
            StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) sb.append(",");
                sb.append(arr[i]);
            }
            return sb.append("]").toString();
        }
        if (obj instanceof double[]) {
            double[] arr = (double[]) obj;
            StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) sb.append(",");
                sb.append(arr[i]);
            }
            return sb.append("]").toString();
        }
        if (obj instanceof String[]) {
            String[] arr = (String[]) obj;
            StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) sb.append(",");
                sb.append("\"").append(arr[i]).append("\"");
            }
            return sb.append("]").toString();
        }
        if (obj instanceof List) {
            List<?> list = (List<?>) obj;
            StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(",");
                sb.append(toJson(list.get(i)));
            }
            return sb.append("]").toString();
        }
        return "\"" + obj.toString() + "\"";
    }"""

        code = code.replace(
            """    private static String toJson(Object obj) {
        return new Gson().toJson(obj);
    }""",
            simple_json,
        )

        return code
