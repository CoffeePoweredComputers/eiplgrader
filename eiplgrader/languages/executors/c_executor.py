"""C language executor for code testing."""

import os
import json
from typing import Dict, Any, List
from .base_executors import CompiledLanguageExecutor


class CExecutor(CompiledLanguageExecutor):
    """Executor for C language code testing."""

    def __init__(self):
        super().__init__(
            compile_cmd=["gcc"], run_cmd=None, file_ext=".c", use_json_input=False
        )

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare C code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        parameter_types = test_case.get("parameter_types")
        expected_type = test_case.get("expected_type")
        inplace_mode = test_case.get("inplace", "0")

        # Validate required type information using standardized error message
        errors = []
        if not parameter_types:
            errors.append("parameter_types not provided")
        if not expected_type:
            errors.append("expected_type not provided")

        if errors:
            error_msg = "Missing required type information:\n"
            error_msg += "\n".join(f"- {error}" for error in errors) + "\n"
            error_msg += "\nTest case must include:\n"
            error_msg += "{\n"
            error_msg += '    "parameters": {...},\n'
            error_msg += '    "parameter_types": {"param1": "type1", ...},\n'
            error_msg += '    "expected": ...,\n'
            error_msg += '    "expected_type": "type"\n'
            error_msg += "}"
            raise ValueError(error_msg)

        # Validate all parameters have types
        for param_name in parameters:
            if parameter_types is None or param_name not in parameter_types:
                raise ValueError(
                    f"Missing required type information:\n- parameter_types['{param_name}'] not provided"
                )

        # Ensure code has necessary headers
        if "#include <stdio.h>" not in code:
            code = "#include <stdio.h>\n" + code
        if "#include <stdlib.h>" not in code:
            code = "#include <stdlib.h>\n" + code
        if "#include <string.h>" not in code:
            code = "#include <string.h>\n" + code

        # Generate main function with embedded values
        main_code = """\nint main() {
    // Test parameters (embedded values)
"""

        # Generate parameter parsing based on explicit types
        param_names = list(parameters.keys())

        for name in param_names:
            param_type = parameter_types.get(name) if parameter_types else None
            value = parameters[name]
            main_code += self._generate_param_declaration(name, param_type, value)

        # Generate function call based on inplace mode
        main_code += self._generate_function_call(
            function_name,
            param_names,
            expected_type or "",
            inplace_mode,
            parameter_types or {},
            parameters,
        )

        main_code += """
    return 0;
}
"""

        # Combine everything
        return code + "\n" + main_code

    def _generate_param_declaration(
        self, name: str, c_type: str | None, value: Any
    ) -> str:
        """Generate parameter declaration with embedded value."""
        if c_type is None:
            return f"    // Unknown type for {name}\n"
        elif c_type == "int":
            return f"    int {name} = {value};\n"
        elif c_type == "double":
            return f"    double {name} = {value};\n"
        elif c_type == "char*":
            return f'    char {name}[] = "{value}";\n'
        elif c_type == "int*" and isinstance(value, list):
            values_str = ", ".join(str(v) for v in value)
            return f"    int {name}[] = {{{values_str}}};\n"
        elif c_type == "double*" and isinstance(value, list):
            values_str = ", ".join(str(v) for v in value)
            return f"    double {name}[] = {{{values_str}}};\n"
        else:
            return f"    // Unsupported type: {c_type} {name}\n"

    def _generate_function_call(
        self,
        function_name: str,
        param_names: List[str],
        expected_type: str,
        inplace_mode: str,
        parameter_types: Dict[str, str],
        parameters: Dict[str, Any],
    ) -> str:
        """Generate function call and output code."""
        code = ""

        if inplace_mode == "0":
            # Normal function call
            c_return_type = expected_type
            format_spec = self._get_format_spec(c_return_type)

            if c_return_type == "int*":
                # Handle array return
                code += (
                    f"    int* result = {function_name}({', '.join(param_names)});\n"
                )
                code += '    printf("[");\n'
                code += "    // Assume small array for simplicity\n"
                code += "    for (int i = 0; i < 10 && result[i] != 0; i++) {\n"
                code += '        if (i > 0) printf(", ");\n'
                code += '        printf("%d", result[i]);\n'
                code += "    }\n"
                code += '    printf("]\\n");\n'
            else:
                code += f"    {c_return_type} result = {function_name}({', '.join(param_names)});\n"
                code += f'    printf("{format_spec}\\n", result);\n'
        elif inplace_mode == "1":
            # In-place modification
            code += f"    {function_name}({', '.join(param_names)});\n"
            if param_names:
                first_param = param_names[0]
                first_type = parameter_types.get(first_param)
                if not first_type:
                    raise ValueError(f"Type required for parameter '{first_param}'")
                c_type = first_type
                if c_type == "int*":
                    array_size = (
                        len(parameters[first_param])
                        if isinstance(parameters[first_param], list)
                        else 5
                    )
                    code += '    printf("[");\n'
                    code += f"    for (int i = 0; i < {array_size}; i++) {{\n"
                    code += '        if (i > 0) printf(", ");\n'
                    code += f'        printf("%d", {first_param}[i]);\n'
                    code += "    }\n"
                    code += '    printf("]\\n");\n'
                else:
                    format_spec = self._get_format_spec(c_type)
                    code += f'    printf("{format_spec}\\n", {first_param});\n'
            else:
                code += '    printf("null\\n");\n'
        elif inplace_mode == "2":
            # Both modify and return
            c_return_type = expected_type
            format_spec = self._get_format_spec(c_return_type)
            code += f"    {c_return_type} result = {function_name}({', '.join(param_names)});\n"
            code += f'    printf("{format_spec}\\n", result);\n'

        return code

    def _get_format_spec(self, c_type: str) -> str:
        """Get printf format specifier for C type."""
        format_specs = {
            "int": "%d",
            "double": "%.6f",
            "float": "%.6f",
            "char*": '\\"%s\\"',
        }
        return format_specs.get(c_type, "%d")
