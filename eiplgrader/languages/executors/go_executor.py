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
            use_json_input=False,  # Go now uses embedded values
        )

    def _generate_output(self, go_type: str, var_name: str) -> str:
        """Generate output code for a Go variable based on its type."""

        if go_type in ["int", "float64", "string", "bool"]:
            return f"    fmt.Println({var_name})\n"
        elif go_type in [
            "[]int",
            "[]float64",
            "[]string",
            "[]bool",
        ] or go_type.startswith("map"):
            # Use encoding/json to serialize complex types
            return f"""    encoder := json.NewEncoder(os.Stdout)
        encoder.Encode({var_name})
    """
        else:
            # Default fmt.Println for other types
            return f"    fmt.Println({var_name})\n"

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Go code for execution with embedded test values."""
        # Use common validation to ensure types are provided
        self.validate_types_provided(test_case)

        function_name = test_case.get("function_name", "foo")
        params = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")
        param_types = test_case["parameter_types"]  # Required field
        expected_type = test_case["expected_type"]  # Required field

        # Ensure code has package declaration
        if not code.strip().startswith("package"):
            code = "package main\n\n" + code

        # Extract existing imports from code
        import re

        import_pattern = r'^import\s+"([^"]+)"$'
        multi_import_pattern = r"^import\s+\(\s*([^)]+)\s*\)$"
        existing_imports = set()

        lines = code.split("\n")
        new_lines = []
        in_import_block = False

        for line in lines:
            # Single import statement
            single_match = re.match(import_pattern, line.strip())
            if single_match:
                existing_imports.add(single_match.group(1))
                continue  # Skip this line, we'll add imports later

            # Multi-import block start
            if line.strip() == "import (" or re.match(r"^import\s+\($", line.strip()):
                in_import_block = True
                continue

            # Inside import block
            if in_import_block:
                if line.strip() == ")":
                    in_import_block = False
                    continue
                # Extract import from quoted string
                import_match = re.match(r'\s*"([^"]+)"', line)
                if import_match:
                    existing_imports.add(import_match.group(1))
                continue

            new_lines.append(line)

        code = "\n".join(new_lines)

        # Get param names for import determination
        param_names = list(params.keys())

        # Add necessary imports based on what we'll use
        imports_needed = set()

        # Determine if we need fmt
        if inplace_mode == "0":
            # Always need output for return values
            imports_needed.add("fmt")
        elif inplace_mode == "1":
            # Only need fmt if we're outputting the modified value
            if param_names:
                first_param_type = param_types.get(param_names[0], "")
                if first_param_type in ["int", "float64", "string", "bool"]:
                    imports_needed.add("fmt")
                elif first_param_type in [
                    "[]int",
                    "[]float64",
                    "[]string",
                    "[]bool",
                ] or first_param_type.startswith("map"):
                    imports_needed.add("encoding/json")
                    imports_needed.add("os")
            else:
                imports_needed.add("fmt")  # For null output
        elif inplace_mode == "2":
            # Need output for return value
            imports_needed.add("fmt")

        # Check if we need encoding/json for output based on expected_type
        if inplace_mode != "1":  # For modes 0 and 2, check expected_type
            if expected_type in [
                "[]int",
                "[]string",
                "[]float64",
                "[]bool",
            ] or expected_type.startswith("map"):
                imports_needed.add("encoding/json")
                imports_needed.add("os")
                imports_needed.discard("fmt")  # Don't need fmt if using encoding/json

        # Combine with existing imports
        all_imports = sorted(existing_imports.union(imports_needed))

        # Build import block
        if len(all_imports) == 1:
            import_block = f'import "{list(all_imports)[0]}"\n'
        else:
            import_lines = ["import ("]
            for imp in all_imports:
                import_lines.append(f'    "{imp}"')
            import_lines.append(")")
            import_block = "\n".join(import_lines) + "\n"

        # Add imports after package declaration
        lines = code.split("\n")
        for i, line in enumerate(lines):
            if line.strip().startswith("package"):
                lines.insert(i + 1, "")
                lines.insert(i + 2, import_block)
                break
        code = "\n".join(lines)

        # Generate parameter declarations with embedded values
        param_declarations = []

        for param_name in param_names:
            param_value = params[param_name]
            go_type = param_types[param_name]

            if go_type == "string":
                # Escape the string value
                escaped_value = (
                    str(param_value).replace("\\", "\\\\").replace('"', '\\"')
                )
                param_declarations.append(f'    {param_name} := "{escaped_value}"')
            elif go_type == "bool":
                param_declarations.append(
                    f"    {param_name} := {str(param_value).lower()}"
                )
            elif go_type in ["int", "float64"]:
                param_declarations.append(f"    {param_name} := {param_value}")
            elif go_type == "[]int":
                values = ", ".join(str(v) for v in param_value)
                param_declarations.append(f"    {param_name} := []int{{{values}}}")
            elif go_type == "[]float64":
                values = ", ".join(str(v) for v in param_value)
                param_declarations.append(f"    {param_name} := []float64{{{values}}}")
            elif go_type == "[]string":
                values = ", ".join(
                    f'"{str(v).replace("\\", "\\\\").replace('"', '\\"')}"'
                    for v in param_value
                )
                param_declarations.append(f"    {param_name} := []string{{{values}}}")
            elif go_type == "[]bool":
                values = ", ".join(str(v).lower() for v in param_value)
                param_declarations.append(f"    {param_name} := []bool{{{values}}}")
            elif go_type == "[][]int":
                # Handle nested int slices
                inner_slices = []
                for inner_list in param_value:
                    inner_values = ", ".join(str(v) for v in inner_list)
                    inner_slices.append(f"[]int{{{inner_values}}}")
                param_declarations.append(
                    f'    {param_name} := [][]int{{{", ".join(inner_slices)}}}'
                )
            elif go_type == "interface{}":
                # Handle interface{} type - can be any value
                if isinstance(param_value, str):
                    escaped_value = (
                        str(param_value).replace("\\", "\\\\").replace('"', '\\"')
                    )
                    param_declarations.append(
                        f'    {param_name} := interface{{}}("{escaped_value}")'
                    )
                elif isinstance(param_value, bool):
                    param_declarations.append(
                        f"    {param_name} := interface{{}}{str(param_value).lower()}"
                    )
                elif isinstance(param_value, (int, float)):
                    param_declarations.append(
                        f"    {param_name} := interface{{}}{param_value}"
                    )
                else:
                    # For complex types, use a placeholder
                    param_declarations.append(
                        f"    // Complex interface{{}} value for {param_name}"
                    )
            else:
                # For unsupported types, use a placeholder
                param_declarations.append(
                    f"    // Unsupported type {go_type} for {param_name}"
                )

        # Generate main function
        main_code = "\nfunc main() {\n"

        # Add parameter declarations
        if param_declarations:
            main_code += "\n".join(param_declarations) + "\n\n"

        # Generate function call and output handling
        if inplace_mode == "0":
            # Normal function call with return value
            if param_names:
                main_code += (
                    f"    result := {function_name}({', '.join(param_names)})\n"
                )
            else:
                main_code += f"    result := {function_name}()\n"
            main_code += self._generate_output(expected_type, "result")
        elif inplace_mode == "1":
            # In-place modification
            if param_names:
                # For slices, pass by reference
                args = []
                for p in param_names:
                    if param_types[p].startswith("List[") or param_types[p].startswith(
                        "[]"
                    ):
                        args.append(p)  # Slices are already references in Go
                    else:
                        args.append("&" + p)  # Other types need address-of
                main_code += f"    {function_name}({', '.join(args)})\n"
                # Output the first parameter after modification
                first_param = param_names[0]
                main_code += self._generate_output(
                    param_types[first_param], first_param
                )
            else:
                main_code += f"    {function_name}()\n"
                main_code += '    fmt.Println("null")\n'
        elif inplace_mode == "2":
            # Both modify and return
            if param_names:
                args = []
                for p in param_names:
                    if param_types[p].startswith("List[") or param_types[p].startswith(
                        "[]"
                    ):
                        args.append(p)
                    else:
                        args.append("&" + p)
                main_code += f"    result := {function_name}({', '.join(args)})\n"
            else:
                main_code += f"    result := {function_name}()\n"
            main_code += self._generate_output(expected_type, "result")

        main_code += "}\n"

        return code + "\n" + main_code

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute test with embedded values."""
        # No type inference - types must be provided
        result = super().execute_test(code, test_case)

        # Clean up serialized output if needed
        if result.get("output") and not result.get("error"):
            output = result["output"].strip()
            # Handle string outputs that might have extra quotes
            if output.startswith('"') and output.endswith('"') and len(output) > 2:
                try:
                    # Try to parse as serialized string
                    result["actual"] = json.loads(output)
                except Exception:
                    # If it fails, use the raw output
                    result["actual"] = output

        return result
