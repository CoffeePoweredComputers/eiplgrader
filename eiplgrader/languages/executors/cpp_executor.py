"""C++ language executor for code testing."""

from typing import Dict, Any
from .base_executors import CompiledLanguageExecutor
from .string_utils import CodeBuilder
from .templates import (
    generate_cpp_param_declaration,
    generate_cpp_output,
)


class CppExecutor(CompiledLanguageExecutor):
    """Executor for C++ language code testing."""

    def __init__(self):
        super().__init__(
            compile_cmd=["g++", "-std=c++17"], run_cmd=["./a.out"], file_ext=".cpp"
        )

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare C++ code for execution with test harness."""
        # Use common validation
        self.validate_types_provided(test_case)

        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        parameter_types = test_case[
            "parameter_types"
        ]  # Required field after validation
        expected_type = test_case["expected_type"]  # Required field after validation
        inplace_mode = test_case.get("inplace", "0")

        # Ensure necessary headers are included using CodeBuilder
        necessary_includes = [
            "#include <iostream>",
            "#include <vector>",
            "#include <string>",
            "#include <algorithm>",
        ]

        lines = code.split("\n")
        includes_to_add = []

        # Find where includes end
        include_section_end = 0
        for i, line in enumerate(lines):
            if line.strip().startswith("#include"):
                include_section_end = i + 1
            elif line.strip() and not line.strip().startswith("//"):
                break

        # Add missing includes
        existing_includes = "\n".join(lines[:include_section_end])
        for inc in necessary_includes:
            if inc not in existing_includes:
                includes_to_add.append(inc)

        # Rebuild code with all includes using CodeBuilder
        builder = CodeBuilder()
        if include_section_end > 0:
            # Add existing includes
            for line in lines[:include_section_end]:
                builder.add_line(line)
            # Add missing includes
            for inc in includes_to_add:
                builder.add_line(inc)
            builder.add_line()  # Empty line
            # Add rest of code
            for line in lines[include_section_end:]:
                builder.add_line(line)
            code = builder.build()
        else:
            # Add all necessary includes first
            for inc in necessary_includes:
                builder.add_line(inc)
            builder.add_line()  # Empty line
            builder.add_line(code)
            code = builder.build()

        # Generate main function with embedded test values using CodeBuilder
        main_builder = CodeBuilder()
        main_builder.add_line()
        main_builder.add_line("int main() {")

        with main_builder.indent():
            main_builder.add_line("// Test case values")

            # Generate parameter declarations with embedded values
            param_names = list(parameters.keys())

            for name in param_names:
                param_type = parameter_types.get(name)
                if not param_type:
                    raise ValueError(f"Type required for parameter '{name}'")

                value = parameters[name]
                param_decl = self._generate_param_declaration(name, param_type, value)
                main_builder.add_line(param_decl.strip())

            main_builder.add_line()

            # Generate function call based on inplace mode
            if inplace_mode == "0":
                # Normal function call - function returns a value
                main_builder.add_line(
                    f"{expected_type} result = {function_name}({', '.join(param_names)});"
                )
                output_code = self._generate_output(expected_type, "result")
                main_builder.add_lines(output_code.rstrip().split("\n"))
            elif inplace_mode == "1":
                # Function modifies arguments in-place
                main_builder.add_line(f"{function_name}({', '.join(param_names)});")
                if param_names:
                    first_param = param_names[0]
                    first_type = parameter_types.get(first_param)
                    output_code = self._generate_output(first_type, first_param)
                    main_builder.add_lines(output_code.rstrip().split("\n"))
                else:
                    main_builder.add_line('std::cout << "null" << std::endl;')
            elif inplace_mode == "2":
                # Function both modifies and returns
                main_builder.add_line(
                    f"{expected_type} result = {function_name}({', '.join(param_names)});"
                )
                output_code = self._generate_output(expected_type, "result")
                main_builder.add_lines(output_code.rstrip().split("\n"))

            main_builder.add_line("return 0;")

        main_builder.add_line("}")
        main_code = main_builder.build()

        return code + "\n" + main_code

    def _generate_param_declaration(
        self, name: str, param_type: str, value: Any
    ) -> str:
        """Generate C++ parameter declaration with embedded value using templates."""
        # Try template function first
        template_result = generate_cpp_param_declaration(name, param_type, value)

        # If template doesn't support the type, fall back to original implementation
        if template_result.strip().startswith("// Unsupported type"):
            # Handle complex types using original logic
            if param_type in ["int", "long", "short"]:
                return f"    {param_type} {name} = {value};\n"
            elif param_type in ["double", "float"]:
                return f"    {param_type} {name} = {value};\n"
            elif param_type == "char":
                return f"    char {name} = '{value}';\n"
            elif param_type == "bool":
                cpp_bool = "true" if value else "false"
                return f"    bool {name} = {cpp_bool};\n"
            elif param_type in ["std::string", "string"]:
                return f'    std::string {name} = "{value}";\n'
            elif "vector" in param_type and isinstance(value, list):
                # Check if it's a nested vector
                if (
                    "vector<vector" in param_type
                    or "std::vector<std::vector" in param_type
                ):
                    # Handle nested vectors
                    inner_vectors = []
                    for inner_list in value:
                        if isinstance(inner_list, list):
                            if "int" in param_type:
                                inner_str = ", ".join(str(v) for v in inner_list)
                            elif "double" in param_type or "float" in param_type:
                                inner_str = ", ".join(str(v) for v in inner_list)
                            elif "string" in param_type:
                                inner_str = ", ".join(f'"{v}"' for v in inner_list)
                            elif "bool" in param_type:
                                inner_str = ", ".join(
                                    "true" if v else "false" for v in inner_list
                                )
                            else:
                                inner_str = ", ".join(str(v) for v in inner_list)
                            inner_vectors.append(f"{{{inner_str}}}")
                        else:
                            inner_vectors.append(str(inner_list))
                    values_str = ", ".join(inner_vectors)
                else:
                    # Handle regular vectors
                    if "int" in param_type:
                        values_str = ", ".join(str(v) for v in value)
                    elif "double" in param_type or "float" in param_type:
                        values_str = ", ".join(str(v) for v in value)
                    elif "string" in param_type:
                        values_str = ", ".join(f'"{v}"' for v in value)
                    elif "bool" in param_type:
                        values_str = ", ".join("true" if v else "false" for v in value)
                    else:
                        values_str = ", ".join(str(v) for v in value)
                return f"    {param_type} {name} = {{{values_str}}};\n"
            else:
                # For complex types, generate a comment
                return (
                    f"    // TODO: Initialize {param_type} {name} with value {value}\n"
                )
        else:
            # Template function supports this type, add newline if needed
            return (
                template_result
                if template_result.endswith("\n")
                else template_result + "\n"
            )

    def _generate_output(self, cpp_type: str, var_name: str) -> str:
        """Generate output code for a variable of given type using templates."""
        # Try template function first
        template_result = generate_cpp_output(cpp_type, var_name)

        # Check if template handles boolean output correctly
        if (
            cpp_type == "bool"
            and f"std::cout << {var_name} << std::endl;" in template_result
        ):
            # Template doesn't handle boolean correctly, use original logic
            return f'    std::cout << ({var_name} ? "true" : "false") << std::endl;\n'
        elif "vector" in cpp_type and cpp_type not in [
            "std::vector<int>",
            "std::vector<std::string>",
            "std::vector<double>",
        ]:
            # For unsupported vector types, fall back to original implementation
            code = f'    std::cout << "[";\n'
            code += f"    for(size_t i = 0; i < {var_name}.size(); i++) {{\n"
            code += f'        if(i > 0) std::cout << ",";\n'
            if "string" in cpp_type:
                code += f'        std::cout << "\\"" << {var_name}[i] << "\\"";\n'
            else:
                code += f"        std::cout << {var_name}[i];\n"
            code += f"    }}\n"
            code += f'    std::cout << "]" << std::endl;\n'
            return code
        else:
            # Template function works, add proper indentation and newline
            lines = template_result.strip().split("\n")
            indented_lines = ["    " + line for line in lines]
            return "\n".join(indented_lines) + "\n"
