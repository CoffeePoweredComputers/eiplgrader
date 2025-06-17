"""Kotlin language executor for code testing."""

import json
import os
import subprocess
import tempfile
from typing import Dict, Any, Tuple
from ..executors.base_executors import CompiledLanguageExecutor


class KotlinExecutor(CompiledLanguageExecutor):
    """Executor for Kotlin language code testing."""

    def __init__(self):
        super().__init__(compile_cmd=["kotlinc"], run_cmd=["kotlin"], file_ext=".kt")
        self.use_script_mode = False  # Can switch to script mode for .kts files

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Kotlin code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        params = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # Build parameter parsing code
        param_names = list(params.keys())
        param_parsing = ""
        param_list = []

        for i, param_name in enumerate(param_names):
            param_value = params[param_name]
            param_type = self._infer_kotlin_type(param_value)
            param_parsing += f"    val {param_name} = args[{i}].{self._get_parse_method(param_type)}\n"
            param_list.append(param_name)

        # Build function call based on inplace mode
        if inplace_mode == "0":
            # Normal function call - returns a value
            function_call = f"{function_name}({', '.join(param_list)})"
            result_handling = (
                f"    val result = {function_call}\n    println(toJson(result))"
            )
        elif inplace_mode == "1":
            # In-place modification (for arrays/lists)
            if param_list:
                first_param = param_list[0]
                other_params = ", ".join(param_list[1:]) if len(param_list) > 1 else ""
                if other_params:
                    function_call = f"{function_name}({first_param}, {other_params})"
                else:
                    function_call = f"{function_name}({first_param})"
                result_handling = f"""    val mutableParam = {first_param}.toMutableList()
    {function_call.replace(first_param, 'mutableParam')}
    println(toJson(mutableParam))"""
            else:
                function_call = f"{function_name}()"
                result_handling = (
                    f"    val result = {function_call}\n    println(toJson(result))"
                )
        elif inplace_mode == "2":
            # Both modifies and returns
            if param_list:
                first_param = param_list[0]
                other_params = ", ".join(param_list[1:]) if len(param_list) > 1 else ""
                if other_params:
                    function_call = f"{function_name}({first_param}, {other_params})"
                else:
                    function_call = f"{function_name}({first_param})"
                result_handling = f"""    val mutableParam = {first_param}.toMutableList()
    val result = {function_call.replace(first_param, 'mutableParam')}
    println(toJson(result ?: mutableParam))"""
            else:
                function_call = f"{function_name}()"
                result_handling = (
                    f"    val result = {function_call}\n    println(toJson(result))"
                )
        else:
            result_handling = '    println("Error: Invalid inplace mode")'

        # Build complete test harness
        test_harness = f"""import com.google.gson.Gson

{code}

fun toJson(obj: Any?): String {{
    return Gson().toJson(obj)
}}

fun main(args: Array<String>) {{
    // Parse command line arguments
{param_parsing}
    
    // Call function and handle result
{result_handling}
}}
"""

        return test_harness

    def _infer_kotlin_type(self, value: Any) -> str:
        """Infer Kotlin type from Python value."""
        if isinstance(value, bool):
            return "Boolean"
        elif isinstance(value, int):
            return "Int"
        elif isinstance(value, float):
            return "Double"
        elif isinstance(value, str):
            return "String"
        elif isinstance(value, list):
            if value and isinstance(value[0], int):
                return "List<Int>"
            elif value and isinstance(value[0], float):
                return "List<Double>"
            else:
                return "List<String>"
        else:
            return "String"

    def _get_parse_method(self, kotlin_type: str) -> str:
        """Get the appropriate parse method for a Kotlin type."""
        type_mapping = {
            "Int": "toInt()",
            "Double": "toDouble()",
            "Boolean": "toBoolean()",
            "String": "",  # No conversion needed
            "List<Int>": "split(',').map { it.trim().toInt() }",
            "List<Double>": "split(',').map { it.trim().toDouble() }",
            "List<String>": "split(',').map { it.trim() }",
        }
        return type_mapping.get(kotlin_type, "")

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile Kotlin code with classpath for Gson."""
        output_dir = os.path.dirname(code_path)
        class_name = (
            os.path.splitext(os.path.basename(code_path))[0].capitalize() + "Kt"
        )

        # Try to find Gson in common locations
        gson_paths = [
            "/usr/share/java/gson.jar",
            "/usr/local/share/java/gson.jar",
            os.path.expanduser(
                "~/.m2/repository/com/google/code/gson/gson/2.8.9/gson-2.8.9.jar"
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
            cmd = [
                "kotlinc",
                "-cp",
                gson_jar,
                code_path,
                "-include-runtime",
                "-d",
                f"{output_dir}/output.jar",
            ]
        else:
            # Fallback: compile without Gson and use simple JSON serialization
            cmd = [
                "kotlinc",
                code_path,
                "-include-runtime",
                "-d",
                f"{output_dir}/output.jar",
            ]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return True, f"{output_dir}/output.jar", ""
        else:
            # If Gson compilation fails, try without it
            if gson_jar and "gson" in result.stderr.lower():
                cmd = [
                    "kotlinc",
                    code_path,
                    "-include-runtime",
                    "-d",
                    f"{output_dir}/output.jar",
                ]
                result = subprocess.run(cmd, capture_output=True, text=True)
                if result.returncode == 0:
                    return True, f"{output_dir}/output.jar", ""

            return False, "", result.stderr

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Compile and execute Kotlin test."""
        # Check if we need to use a simpler version without Gson
        if "import com.google.gson.Gson" in code:
            # Try with Gson first
            result = self._execute_with_gson(code, test_case)
            if (
                result.get("error", "").startswith("Compilation failed")
                and "gson" in result.get("error", "").lower()
            ):
                # Fallback to simple JSON
                code = self._convert_to_simple_json(code)
                return super().execute_test(code, test_case)
            return result
        else:
            return super().execute_test(code, test_case)

    def _execute_with_gson(
        self, code: str, test_case: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Try to execute with Gson support."""
        # Prepare code with test harness
        prepared_code = self.prepare_code(code, test_case)

        # Write to temporary file
        code_path = os.path.join(self.temp_dir, f"test{self.file_ext}")
        with open(code_path, "w") as f:
            f.write(prepared_code)

        # Compile
        success, output_path, error = self.compile(code_path)
        if not success:
            # If Gson compilation fails, convert to simple JSON
            prepared_code = self._convert_to_simple_json(prepared_code)
            with open(code_path, "w") as f:
                f.write(prepared_code)
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
                    "~/.m2/repository/com/google/code/gson/gson/2.8.9/gson-2.8.9.jar"
                ),
            ]
            for path in gson_paths:
                if os.path.exists(path):
                    gson_jar = path
                    break

            if gson_jar and "Gson" in prepared_code:
                cmd = ["java", "-cp", f"{output_path}:{gson_jar}", "TestKt"] + args
            else:
                cmd = ["java", "-jar", output_path] + args

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
        code = code.replace("import com.google.gson.Gson\n", "")

        # Replace Gson toJson with simple implementation
        simple_json = """
fun toJson(obj: Any?): String {
    return when (obj) {
        null -> "null"
        is Boolean -> obj.toString()
        is Number -> obj.toString()
        is String -> "\"$obj\""
        is List<*> -> "[" + obj.joinToString(",") { toJson(it) } + "]"
        is Array<*> -> "[" + obj.joinToString(",") { toJson(it) } + "]"
        else -> "\"${obj.toString()}\""
    }
}
"""

        code = code.replace(
            """fun toJson(obj: Any?): String {
    return Gson().toJson(obj)
}""",
            simple_json,
        )

        return code
