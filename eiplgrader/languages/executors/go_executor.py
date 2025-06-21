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
        from .templates import generate_go_output
        
        return generate_go_output(go_type, var_name)

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Go code for execution with embedded test values."""
        from .string_utils import CodeBuilder
        from .templates import generate_go_param_declaration, generate_inplace_function_call
        
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

        # Build import block using CodeBuilder
        import_builder = CodeBuilder()
        if len(all_imports) == 1:
            import_builder.add_line(f'import "{list(all_imports)[0]}"')
        else:
            import_builder.add_line("import (")
            with import_builder.indent():
                for imp in all_imports:
                    import_builder.add_line(f'"{imp}"')
            import_builder.add_line(")")

        # Add imports after package declaration
        lines = code.split("\n")
        for i, line in enumerate(lines):
            if line.strip().startswith("package"):
                lines.insert(i + 1, "")
                lines.insert(i + 2, import_builder.build())
                break
        code = "\n".join(lines)

        # Generate parameter declarations using templates
        param_declarations = []
        for param_name in param_names:
            param_value = params[param_name]
            go_type = param_types[param_name]
            
            # Handle special cases not covered by the template
            if go_type == "[][]int":
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
                        f'    {param_name} := interface{{}}(\"{escaped_value}\")'
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
                # Use the template function for standard types
                param_declarations.append(
                    generate_go_param_declaration(param_name, go_type, param_value)
                )

        # Generate main function using CodeBuilder
        main_builder = CodeBuilder()
        main_builder.add_line("func main() {")
        
        # Add parameter declarations
        with main_builder.indent():
            if param_declarations:
                for decl in param_declarations:
                    # Remove the leading spaces since CodeBuilder handles indentation
                    clean_decl = decl.strip()
                    main_builder.add_line(clean_decl)
                main_builder.add_line("")

            # Generate function call using templates
            function_call = generate_inplace_function_call(
                "go", function_name, param_names, inplace_mode, param_types, expected_type
            )
            main_builder.add_lines(function_call)

        main_builder.add_line("}")

        return code + "\n" + main_builder.build()

    def normalize_output(self, raw_output: str, expected_type: str = None) -> Any:
        """Handle Go-specific output parsing."""
        output = raw_output.strip()
        
        # Handle string outputs that might have extra quotes
        if output.startswith('"') and output.endswith('"') and len(output) > 2:
            try:
                # Try to parse as serialized string
                return json.loads(output)
            except Exception:
                # If it fails, fall back to parent normalize_output
                pass
        
        # Otherwise call parent normalize_output for default behavior
        return super().normalize_output(raw_output, expected_type)
