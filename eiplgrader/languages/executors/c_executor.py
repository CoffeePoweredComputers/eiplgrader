"""C language executor for code testing."""

from typing import Dict, Any, List, Optional
from .base_executors import CompiledLanguageExecutor
from .string_utils import CodeBuilder
from .templates import (
    generate_c_param_declaration,
    generate_c_output,
)


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

        # Validate required type information using CodeBuilder
        errors = []
        if not parameter_types:
            errors.append("parameter_types not provided")
        if not expected_type:
            errors.append("expected_type not provided")

        if errors:
            error_builder = CodeBuilder()
            error_builder.add_line("Missing required type information:")
            for error in errors:
                error_builder.add_line(f"- {error}")
            error_builder.add_line("")
            error_builder.add_line("Test case must include:")
            error_builder.add_line("{")
            with error_builder.indent():
                error_builder.add_line('"parameters": {...},')
                error_builder.add_line('"parameter_types": {"param1": "type1", ...},')
                error_builder.add_line('"expected": ...,')
                error_builder.add_line('"expected_type": "type"')
            error_builder.add_line("}")
            raise ValueError(error_builder.build())

        # Validate all parameters have types
        if parameter_types is None:
            raise ValueError("Missing required type information")

        for param_name in parameters:
            if param_name not in parameter_types:
                raise ValueError(
                    f"Missing required type information:\n- parameter_types['{param_name}'] not provided"
                )

        # Ensure code has necessary headers using organized approach
        headers = ["stdio.h", "stdlib.h", "string.h"]
        header_builder = CodeBuilder()

        # Add missing headers
        for header in headers:
            include_line = f"#include <{header}>"
            if include_line not in code:
                header_builder.add_line(include_line)

        # Prepend headers to code if any were missing
        if len(header_builder) > 0:
            code = header_builder.build() + "\n" + code

        # Generate main function with embedded values using CodeBuilder
        main_builder = CodeBuilder()
        main_builder.add_line("")
        main_builder.add_line("int main() {")

        with main_builder.indent():
            main_builder.add_line("// Test parameters (embedded values)")

            # Generate parameter declarations using template function
            param_names = list(parameters.keys())
            for name in param_names:
                param_type = parameter_types.get(name) if parameter_types else None
                value = parameters[name]
                param_decl = generate_c_param_declaration(name, param_type, value)
                main_builder.add_lines(param_decl)

            # Generate function call using simplified approach
            function_call_code = self._generate_function_call(
                function_name,
                param_names,
                expected_type or "",
                inplace_mode,
                parameter_types or {},
                parameters,
            )
            main_builder.add_lines(function_call_code)

            main_builder.add_line("return 0;")

        main_builder.add_line("}")

        # Combine everything
        return code + "\n" + main_builder.build()

    def _generate_param_declaration(
        self, name: str, c_type: str | None, value: Any
    ) -> str:
        """Generate parameter declaration with embedded value.

        This method is maintained for backward compatibility but now
        delegates to the template function for consistency.
        """
        return generate_c_param_declaration(name, c_type, value)

    def _generate_function_call(
        self,
        function_name: str,
        param_names: List[str],
        expected_type: str,
        inplace_mode: str,
        parameter_types: Dict[str, str],
        parameters: Dict[str, Any],
    ) -> str:
        """Generate function call and output code using templates."""
        builder = CodeBuilder()

        # Handle different inplace modes
        if inplace_mode == "0":
            # Normal function call with return value
            if expected_type == "int*":
                # Special case for array return - use template with loop
                builder.add_line(
                    f"int* result = {function_name}({', '.join(param_names)});"
                )
                builder.add_lines(self._generate_array_output_loop("result", "int"))
            else:
                builder.add_line(
                    f"{expected_type} result = {function_name}({', '.join(param_names)});"
                )
                builder.add_lines(generate_c_output(expected_type, "result"))

        elif inplace_mode == "1":
            # In-place modification
            builder.add_line(f"{function_name}({', '.join(param_names)});")
            if param_names:
                first_param = param_names[0]
                first_type = parameter_types.get(first_param)
                if not first_type:
                    raise ValueError(f"Type required for parameter '{first_param}'")

                if first_type == "int*":
                    # Use template for array output
                    array_size = (
                        len(parameters[first_param])
                        if isinstance(parameters[first_param], list)
                        else 5
                    )
                    builder.add_lines(
                        self._generate_array_output_loop(first_param, "int", array_size)
                    )
                else:
                    builder.add_lines(generate_c_output(first_type, first_param))
            else:
                builder.add_line('printf("null\\n");')

        elif inplace_mode == "2":
            # Both modify and return
            builder.add_line(
                f"{expected_type} result = {function_name}({', '.join(param_names)});"
            )
            builder.add_lines(generate_c_output(expected_type, "result"))

        return builder.build()

    def _generate_array_output_loop(
        self, var_name: str, element_type: str, size: Optional[int] = None
    ) -> str:
        """Generate array output loop using template approach."""
        builder = CodeBuilder()

        builder.add_line('printf("[");')
        if size is not None:
            # Fixed size array
            builder.add_line(f"for (int i = 0; i < {size}; i++) {{")
        else:
            # Dynamic size (assuming null-terminated or small array)
            builder.add_line(f"for (int i = 0; i < 10 && {var_name}[i] != 0; i++) {{")

        with builder.indent():
            builder.add_line('if (i > 0) printf(", ");')
            if element_type == "int":
                builder.add_line(f'printf("%d", {var_name}[i]);')
            elif element_type == "double":
                builder.add_line(f'printf("%f", {var_name}[i]);')
            else:
                builder.add_line(f'printf("%d", {var_name}[i]);')

        builder.add_line("}")
        builder.add_line('printf("]\\n");')

        return builder.build()

    def _get_format_spec(self, c_type: str) -> str:
        """Get printf format specifier for C type.

        This method is maintained for backward compatibility.
        """
        format_specs = {
            "int": "%d",
            "double": "%.6f",
            "float": "%.6f",
            "char*": '\\"%s\\"',
        }
        return format_specs.get(c_type, "%d")
