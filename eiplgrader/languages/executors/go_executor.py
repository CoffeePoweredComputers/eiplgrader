"""Go language executor for code testing."""

import json
import re
from typing import Dict, Any
from .base_executors import CompiledLanguageExecutor


class GoExecutor(CompiledLanguageExecutor):
    """Executor for Go language code testing."""

    def __init__(self):
        super().__init__(
            compile_cmd=["go", "build"],
            run_cmd=["go", "run"],
            file_ext=".go",
            use_json_input=False,  # Go uses embedded values
        )

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Go code for execution with embedded test values."""
        from .string_utils import CodeBuilder
        from .templates import generate_go_param_declaration, generate_go_output
        
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
        import_pattern = r'^import\s+"([^"]+)"$'
        multi_import_pattern = r"^import\s+\(\s*([^)]+)\s*\)$"
        
        existing_imports = set()
        lines = code.split('\n')
        
        # Find existing imports
        for line in lines:
            line = line.strip()
            single_match = re.match(import_pattern, line)
            if single_match:
                existing_imports.add(single_match.group(1))
            
            multi_match = re.match(multi_import_pattern, line, re.DOTALL)
            if multi_match:
                import_block = multi_match.group(1)
                for import_line in import_block.split('\n'):
                    import_line = import_line.strip().strip('"')
                    if import_line:
                        existing_imports.add(import_line)

        # Determine what imports we actually need based on what will be generated
        required_imports = set()
        
        # Determine if we need fmt, json, and os based on actual usage
        needs_fmt = False
        needs_json = False
        needs_os = False
        
        # Check what the OUTPUT will use (expected_type determines output format)
        output_type = expected_type
        if (output_type.startswith("[]") or 
            output_type.startswith("map") or 
            (output_type.startswith("(") and output_type.endswith(")"))):  # tuple types
            needs_json = True
            needs_os = True
        else:
            needs_fmt = True
        
        # For inplace mode 1, check the first parameter type for output
        if inplace_mode == "1" and params:
            first_param_name = list(params.keys())[0]
            first_param_type = param_types[first_param_name]
            if (first_param_type.startswith("[]") or 
                first_param_type.startswith("map") or 
                (first_param_type.startswith("(") and first_param_type.endswith(")"))):
                needs_json = True
                needs_os = True
                needs_fmt = False  # Override fmt if using JSON
            else:
                needs_fmt = True
        
        # For inplace mode 1 with no params, we always need fmt for null output
        if inplace_mode == "1" and not params:
            needs_fmt = True
        
        if needs_fmt:
            required_imports.add("fmt")
        if needs_json:
            required_imports.add("encoding/json")
        if needs_os:
            required_imports.add("os")
        
        # Build the final code with proper imports
        builder = CodeBuilder()
        builder.add_line("package main")
        builder.add_line()
        
        # Combine existing and required imports, but avoid duplicates
        all_imports = existing_imports | required_imports
        
        if all_imports:
            builder.add_line("import (")
            with builder.indent():
                for imp in sorted(all_imports):
                    builder.add_line(f'"{imp}"')
            builder.add_line(")")
            builder.add_line()

        # Remove existing package and import declarations from code
        code_lines = []
        skip_mode = False
        import_depth = 0
        
        for line in code.split('\n'):
            stripped = line.strip()
            
            if stripped.startswith('package '):
                continue
            if stripped.startswith('import ('):
                skip_mode = True
                import_depth = 1
                continue
            if stripped.startswith('import '):
                continue
            if skip_mode:
                if '(' in stripped:
                    import_depth += stripped.count('(')
                if ')' in stripped:
                    import_depth -= stripped.count(')')
                if import_depth <= 0:
                    skip_mode = False
                continue
            code_lines.append(line)
        
        # Add the cleaned code
        clean_code = '\n'.join(code_lines).strip()
        if clean_code:
            builder.add_line(clean_code)
            builder.add_line()

        # Generate main function
        builder.add_line("func main() {")
        with builder.indent():
            # Generate parameter declarations with embedded values
            param_names = list(params.keys())
            for param_name in param_names:
                param_value = params[param_name]
                param_type = param_types[param_name]
                declaration = generate_go_param_declaration(param_name, param_type, param_value)
                builder.add_line(declaration)
            
            if param_names:
                builder.add_line()

            # Generate function call and output based on inplace mode
            if inplace_mode == "0":
                # Normal function call - returns a value
                if param_names:
                    function_call = f"{function_name}({', '.join(param_names)})"
                else:
                    function_call = f"{function_name}()"
                builder.add_line(f"result := {function_call}")
                output_code = generate_go_output(expected_type, "result")
                builder.add_lines(output_code.rstrip().split('\n'))
                
            elif inplace_mode == "1":
                # Function modifies arguments in-place
                if param_names:
                    function_call = f"{function_name}({', '.join(param_names)})"
                    builder.add_line(function_call)
                    # Output the first parameter (usually the modified one)
                    first_param = param_names[0]
                    first_type = param_types[first_param]
                    output_code = generate_go_output(first_type, first_param)
                    builder.add_lines(output_code.rstrip().split('\n'))
                else:
                    builder.add_line(f"{function_name}()")
                    builder.add_line('fmt.Println("null")')
                    
            elif inplace_mode == "2":
                # Function both modifies and returns
                if param_names:
                    function_call = f"{function_name}({', '.join(param_names)})"
                else:
                    function_call = f"{function_name}()"
                builder.add_line(f"result := {function_call}")
                output_code = generate_go_output(expected_type, "result")
                builder.add_lines(output_code.rstrip().split('\n'))
            else:
                builder.add_line('fmt.Println("Error: Invalid inplace mode")')

        builder.add_line("}")
        
        return builder.build()

    def normalize_output(self, raw_output: str, expected_type: str = None) -> Any:
        """Normalize Go output to expected format."""
        output = raw_output.strip()
        
        # Try to parse as JSON first for complex types
        if expected_type.startswith("[") or expected_type.startswith("map"):
            try:
                return json.loads(output)
            except (json.JSONDecodeError, ValueError):
                # Fall back to string parsing if JSON fails
                pass
        
        # Handle basic types
        if expected_type == "bool":
            return output == "true"
        elif expected_type == "int":
            try:
                return int(output)
            except ValueError:
                return output
        elif expected_type == "float64":
            try:
                return float(output)
            except ValueError:
                return output
        elif expected_type == "string":
            return output
        else:
            # For unknown types, try JSON parsing first, then return as string
            try:
                return json.loads(output)
            except (json.JSONDecodeError, ValueError):
                return output