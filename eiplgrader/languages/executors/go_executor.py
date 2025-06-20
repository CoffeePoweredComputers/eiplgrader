"""Go language executor for code testing."""

import json
from typing import Dict, Any
from .base_executors import CompiledLanguageExecutor


class GoExecutor(CompiledLanguageExecutor):
    """Executor for Go language code testing."""

    def __init__(self):
        super().__init__(
            compile_cmd=["go", "build"],
            run_cmd=["go", "run"],
            file_ext=".go",
            use_json_input=True,  # Go has native JSON support
        )

    def _to_go_type(self, generic_type: str) -> str:
        """Convert generic type names to Go-specific types."""
        type_mapping = {
            "double": "float64",
            "List[int]": "[]int",
            "List[double]": "[]float64",
            "List[string]": "[]string",
        }
        return type_mapping.get(generic_type, generic_type)

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Go code for execution with JSON-based test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # Ensure code has package declaration
        if not code.strip().startswith("package"):
            code = "package main\n\n" + code

        # Add necessary imports if not present
        imports_needed = ["fmt", "encoding/json", "os"]

        import_lines = [f'    "{imp}"' for imp in imports_needed]
        import_block = "import (\n" + "\n".join(import_lines) + "\n)\n"

        # Check if code already has imports and merge them
        lines = code.split("\n")
        has_import = False
        import_insert_idx = 1

        for i, line in enumerate(lines):
            if line.strip().startswith("package"):
                import_insert_idx = i + 1
            elif line.strip().startswith("import"):
                has_import = True
                # Check if it's a single import or import block
                if "(" in line:
                    # Multi-line import block, find the end
                    for j in range(i + 1, len(lines)):
                        if ")" in lines[j]:
                            # Insert our imports before the closing )
                            for imp in imports_needed:
                                if f'"{imp}"' not in code:
                                    lines.insert(j, f'    "{imp}"')
                            break
                else:
                    # Single import, convert to multi-import
                    existing_import = line.strip().replace("import ", "").strip()
                    lines[i] = "import ("
                    lines.insert(i + 1, f"    {existing_import}")
                    for imp in imports_needed:
                        if f'"{imp}"' not in existing_import:
                            lines.insert(i + 2, f'    "{imp}"')
                    lines.insert(i + 2 + len(imports_needed), ")")
                break

        if not has_import:
            lines.insert(import_insert_idx, "")
            lines.insert(import_insert_idx + 1, import_block)

        code = "\n".join(lines)

        # Generate main function with JSON unmarshaling
        main_code = "\nfunc main() {\n"

        # Read JSON input
        main_code += "    // Read JSON input\n"
        main_code += "    var params map[string]interface{}\n"
        main_code += "    decoder := json.NewDecoder(os.Stdin)\n"
        main_code += "    if err := decoder.Decode(&params); err != nil {\n"
        main_code += '        fmt.Fprintf(os.Stderr, "JSON decode error: %v\\n", err)\n'
        main_code += "        return\n"
        main_code += "    }\n\n"

        # Extract parameters with type assertions
        param_names = list(parameters.keys())
        for name, value in parameters.items():
            # Infer type and convert to Go-specific names
            inferred_type = self.infer_type(value)
            go_type = self._to_go_type(inferred_type)

            if go_type == "int":
                main_code += f'    {name} := int(params["{name}"].(float64))\n'
            elif go_type == "float64":
                main_code += f'    {name} := params["{name}"].(float64)\n'
            elif go_type == "string":
                main_code += f'    {name} := params["{name}"].(string)\n'
            elif go_type == "bool":
                main_code += f'    {name} := params["{name}"].(bool)\n'
            elif go_type == "[]int":
                main_code += (
                    f'    {name}Interface := params["{name}"].([]interface{{}})\n'
                )
                main_code += f"    {name} := make([]int, len({name}Interface))\n"
                main_code += f"    for i, v := range {name}Interface {{\n"
                main_code += f"        {name}[i] = int(v.(float64))\n"
                main_code += f"    }}\n"
            elif go_type == "[]float64":
                main_code += (
                    f'    {name}Interface := params["{name}"].([]interface{{}})\n'
                )
                main_code += f"    {name} := make([]float64, len({name}Interface))\n"
                main_code += f"    for i, v := range {name}Interface {{\n"
                main_code += f"        {name}[i] = v.(float64)\n"
                main_code += f"    }}\n"
            elif go_type == "[]string":
                main_code += (
                    f'    {name}Interface := params["{name}"].([]interface{{}})\n'
                )
                main_code += f"    {name} := make([]string, len({name}Interface))\n"
                main_code += f"    for i, v := range {name}Interface {{\n"
                main_code += f"        {name}[i] = v.(string)\n"
                main_code += f"    }}\n"

        main_code += "\n"

        # Generate function call and output
        if inplace_mode == "0":
            # Normal function call with return value
            if param_names:
                main_code += (
                    f"    result := {function_name}({', '.join(param_names)})\n"
                )
            else:
                main_code += f"    result := {function_name}()\n"
            main_code += "    encoder := json.NewEncoder(os.Stdout)\n"
            main_code += "    encoder.Encode(result)\n"
        elif inplace_mode == "1":
            # In-place modification
            if param_names:
                main_code += f"    {function_name}({', '.join('&' + p if isinstance(parameters[p], list) else p for p in param_names)})\n"
                # Output the first parameter after modification
                first_param = param_names[0]
                main_code += "    encoder := json.NewEncoder(os.Stdout)\n"
                main_code += f"    encoder.Encode({first_param})\n"
            else:
                main_code += f"    {function_name}()\n"
                main_code += '    fmt.Println("null")\n'
        elif inplace_mode == "2":
            # Both modify and return
            if param_names:
                main_code += f"    result := {function_name}({', '.join('&' + p if isinstance(parameters[p], list) else p for p in param_names)})\n"
            else:
                main_code += f"    result := {function_name}()\n"
            main_code += "    encoder := json.NewEncoder(os.Stdout)\n"
            main_code += "    encoder.Encode(result)\n"

        main_code += "}\n"

        return code + "\n" + main_code

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute test with JSON input/output."""
        result = super().execute_test(code, test_case)

        # Clean up JSON output if needed
        if result.get("output") and not result.get("error"):
            output = result["output"].strip()
            # Handle string outputs that might have extra quotes
            if output.startswith('"') and output.endswith('"') and len(output) > 2:
                try:
                    # Try to parse as JSON string
                    result["actual"] = json.loads(output)
                except:
                    # If it fails, use the raw output
                    result["actual"] = output

        return result
