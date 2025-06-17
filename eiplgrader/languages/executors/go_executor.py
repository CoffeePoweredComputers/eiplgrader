"""Go language executor for code testing."""

import os
import json
import tempfile
from typing import Dict, Any, Tuple, List
from .base_executors import CompiledLanguageExecutor


class GoExecutor(CompiledLanguageExecutor):
    """Executor for Go language code testing."""

    def __init__(self):
        super().__init__(
            compile_cmd=["go", "build"], run_cmd=["go", "run"], file_ext=".go"
        )

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Go code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # Ensure code has package declaration
        if not code.strip().startswith("package"):
            code = "package main\n\n" + code

        # Add necessary imports if not present
        imports_needed = ["fmt", "encoding/json", "os"]
        if "error" in code and "errors" not in code:
            imports_needed.append("errors")

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

        # Generate main function with test harness
        main_code = f"""
func main() {{
    // Read test parameters from stdin
    var input string
    fmt.Scanln(&input)
    
    var params map[string]interface{{}}
    err := json.Unmarshal([]byte(input), &params)
    if err != nil {{
        fmt.Fprintf(os.Stderr, "Error parsing input: %v\\n", err)
        os.Exit(1)
    }}
    
    // Extract parameters
"""

        # Generate parameter extraction based on test case
        param_names = list(parameters.keys())
        param_types = self._infer_go_types(parameters)

        for i, (name, go_type) in enumerate(zip(param_names, param_types)):
            if go_type == "int":
                main_code += f'    {name} := int(params["{name}"].(float64))\n'
            elif go_type == "float64":
                main_code += f'    {name} := params["{name}"].({go_type})\n'
            elif go_type == "string":
                main_code += f'    {name} := params["{name}"].({go_type})\n'
            elif go_type == "[]int":
                main_code += f"""    {name}Raw := params["{name}"].([]interface{{}})
    {name} := make([]int, len({name}Raw))
    for i, v := range {name}Raw {{
        {name}[i] = int(v.(float64))
    }}
"""
            elif go_type == "[]string":
                main_code += f"""    {name}Raw := params["{name}"].([]interface{{}})
    {name} := make([]string, len({name}Raw))
    for i, v := range {name}Raw {{
        {name}[i] = v.(string)
    }}
"""

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

    def _infer_go_types(self, parameters: Dict[str, Any]) -> List[str]:
        """Infer Go types from parameter values."""
        types = []
        for value in parameters.values():
            if isinstance(value, bool):
                types.append("bool")
            elif isinstance(value, int):
                types.append("int")
            elif isinstance(value, float):
                types.append("float64")
            elif isinstance(value, str):
                types.append("string")
            elif isinstance(value, list):
                if not value:
                    types.append("[]interface{}")
                elif isinstance(value[0], int):
                    types.append("[]int")
                elif isinstance(value[0], str):
                    types.append("[]string")
                elif isinstance(value[0], float):
                    types.append("[]float64")
                else:
                    types.append("[]interface{}")
            else:
                types.append("interface{}")
        return types

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

            # Pass test parameters as stdin
            args_json = json.dumps(test_case.get("parameters", {}))
            result = subprocess.run(
                ["go", "run", code_path],
                input=args_json,
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
