"""Go language executor for code testing."""

import os
import json
import tempfile
from typing import Dict, Any, Tuple, List
from .base_executors import CompiledLanguageExecutor


class GoExecutor(CompiledLanguageExecutor):
    """Executor for Go language code testing."""
    
    def get_type_mapping(self, type_str: str, language: str = "go") -> str:
        """Map generic type strings to Go types."""
        mappings = {
            "int": "int",
            "double": "float64",
            "float": "float32",
            "string": "string",
            "bool": "bool",
            "int[]": "[]int",
            "List[int]": "[]int",
            "List[double]": "[]float64",
            "List[string]": "[]string",
        }
        return mappings.get(type_str, type_str)

    def __init__(self):
        super().__init__(
            compile_cmd=["go", "build"], run_cmd=["go", "run"], file_ext=".go",
            use_json_input=False  # Use embedded values instead of JSON
        )

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Go code for execution with test harness."""
        # For Go, types are now optional - infer if not provided
        if "parameter_types" not in test_case:
            test_case["parameter_types"] = {}
            for param_name, value in test_case.get("parameters", {}).items():
                if isinstance(value, bool):
                    test_case["parameter_types"][param_name] = "bool"
                elif isinstance(value, int):
                    test_case["parameter_types"][param_name] = "int"
                elif isinstance(value, float):
                    test_case["parameter_types"][param_name] = "float64"
                elif isinstance(value, str):
                    test_case["parameter_types"][param_name] = "string"
                elif isinstance(value, list) and value:
                    if isinstance(value[0], int):
                        test_case["parameter_types"][param_name] = "[]int"
                    elif isinstance(value[0], str):
                        test_case["parameter_types"][param_name] = "[]string"
                    elif isinstance(value[0], float):
                        test_case["parameter_types"][param_name] = "[]float64"
        
        if "expected_type" not in test_case:
            expected = test_case.get("expected")
            if isinstance(expected, bool):
                test_case["expected_type"] = "bool"
            elif isinstance(expected, int):
                test_case["expected_type"] = "int"
            elif isinstance(expected, float):
                test_case["expected_type"] = "float64"
            elif isinstance(expected, str):
                test_case["expected_type"] = "string"
            elif isinstance(expected, list) and expected:
                if isinstance(expected[0], int):
                    test_case["expected_type"] = "[]int"
        
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        parameter_types = test_case.get("parameter_types", {})
        expected_type = test_case.get("expected_type")
        inplace_mode = test_case.get("inplace", "0")

        # Ensure code has package declaration
        if not code.strip().startswith("package"):
            code = "package main\n\n" + code

        # Add necessary imports if not present
        imports_needed = ["fmt", "encoding/json"]
        
        import_lines = [f'    "{imp}"' for imp in imports_needed]
        import_block = "import (\n" + "\n".join(import_lines) + "\n)\n"

        # Insert imports after package declaration if not already present
        if "import" not in code:
            lines = code.split("\n")
            for i, line in enumerate(lines):
                if line.strip().startswith("package"):
                    lines.insert(i + 1, "\n" + import_block)
                    break
            code = "\n".join(lines)

        # Generate main function with embedded test values
        main_code = "\nfunc main() {\n"
        
        # Generate parameter declarations with embedded values
        param_names = list(parameters.keys())
        
        for name in param_names:
            go_type = self.get_type_mapping(parameter_types[name], "go")
            value = parameters[name]
            
            # Format the value for Go
            if isinstance(value, bool):
                value_str = "true" if value else "false"
            elif isinstance(value, str):
                value_str = f'"{value}"'
            elif isinstance(value, list):
                if go_type == "[]int":
                    value_str = "[]int{" + ", ".join(str(v) for v in value) + "}"
                elif go_type == "[]string":
                    value_str = "[]string{" + ", ".join(f'"{v}"' for v in value) + "}"
                elif go_type == "[]float64":
                    value_str = "[]float64{" + ", ".join(str(v) for v in value) + "}"
                else:
                    value_str = str(value)
            else:
                value_str = str(value)
            
            main_code += f"    {name} := {value_str}\n"

        # Generate function call based on inplace mode
        if inplace_mode == "0":
            # Normal function call - function returns a value
            main_code += f"""
    // Call the function
    result := {function_name}({', '.join(param_names)})
    
    // Output result as JSON
    output, _ := json.Marshal(result)
    fmt.Println(string(output))
"""
        elif inplace_mode == "1":
            # Function modifies arguments in-place
            if param_names:
                first_param = param_names[0]
                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )
                if other_params:
                    main_code += f"""
    // Call the function (modifies first parameter)
    {function_name}(&{first_param}, {other_params})
    
    // Output modified parameter as JSON
    output, _ := json.Marshal({first_param})
    fmt.Println(string(output))
"""
                else:
                    main_code += f"""
    // Call the function (modifies parameter)
    {function_name}(&{first_param})
    
    // Output modified parameter as JSON
    output, _ := json.Marshal({first_param})
    fmt.Println(string(output))
"""
            else:
                main_code += f"""
    // Call the function
    {function_name}()
    fmt.Println("null")
"""
        elif inplace_mode == "2":
            # Function both modifies in-place and returns a value
            if param_names:
                first_param = param_names[0]
                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )
                if other_params:
                    main_code += f"""
    // Call the function (modifies first parameter and returns value)
    result := {function_name}(&{first_param}, {other_params})
    
    // Output return value (or modified parameter if nil)
    if result != nil {{
        output, _ := json.Marshal(result)
        fmt.Println(string(output))
    }} else {{
        output, _ := json.Marshal({first_param})
        fmt.Println(string(output))
    }}
"""
                else:
                    main_code += f"""
    // Call the function
    result := {function_name}(&{first_param})
    
    // Output return value (or modified parameter if nil)
    if result != nil {{
        output, _ := json.Marshal(result)
        fmt.Println(string(output))
    }} else {{
        output, _ := json.Marshal({first_param})
        fmt.Println(string(output))
    }}
"""
            else:
                main_code += f"""
    // Call the function
    result := {function_name}()
    output, _ := json.Marshal(result)
    fmt.Println(string(output))
"""

        main_code += "}\n"

        # Combine everything
        return code + "\n" + main_code

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile Go code, return (success, output_path, error)"""
        # For Go, we can use go run directly, so we'll override to use that
        # This avoids compilation step and runs directly
        return (True, code_path, "")

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Compile and execute test"""
        # Prepare code with test harness
        prepared_code = self.prepare_code(code, test_case)

        # Write to temporary file
        code_path = os.path.join(self.temp_dir, "test.go")
        with open(code_path, "w") as f:
            f.write(prepared_code)

        # Execute using go run
        try:
            import subprocess

            # No stdin needed for embedded values
            result = subprocess.run(
                ["go", "run", code_path],
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
