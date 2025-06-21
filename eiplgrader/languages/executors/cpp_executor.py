"""C++ language executor for code testing."""

import os
import json
import subprocess
import tempfile
from typing import Dict, Any, Tuple, List
from .base_executors import CompiledLanguageExecutor


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

        # Ensure necessary headers are included
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

        # Rebuild code with all includes
        if include_section_end > 0:
            new_lines = lines[:include_section_end]
            new_lines.extend(includes_to_add)
            new_lines.append("")  # Empty line
            new_lines.extend(lines[include_section_end:])
            code = "\n".join(new_lines)
        else:
            all_includes = "\n".join(necessary_includes)
            code = all_includes + "\n\n" + code

        # Generate main function with embedded test values
        main_code = "\nint main() {\n"
        main_code += "    // Test case values\n"

        # Generate parameter declarations with embedded values
        param_names = list(parameters.keys())

        for name in param_names:
            param_type = parameter_types.get(name)
            if not param_type:
                raise ValueError(f"Type required for parameter '{name}'")

            value = parameters[name]
            main_code += self._generate_param_declaration(name, param_type, value)

        main_code += "\n"

        # Generate function call based on inplace mode
        if inplace_mode == "0":
            # Normal function call - function returns a value
            main_code += f"    {expected_type} result = {function_name}({', '.join(param_names)});\n"
            main_code += self._generate_output(expected_type, "result")
        elif inplace_mode == "1":
            # Function modifies arguments in-place
            main_code += f"    {function_name}({', '.join(param_names)});\n"
            if param_names:
                first_param = param_names[0]
                first_type = parameter_types.get(first_param)
                main_code += self._generate_output(first_type, first_param)
            else:
                main_code += '    std::cout << "null" << std::endl;\n'
        elif inplace_mode == "2":
            # Function both modifies and returns
            main_code += f"    {expected_type} result = {function_name}({', '.join(param_names)});\n"
            main_code += self._generate_output(expected_type, "result")

        main_code += "    return 0;\n}\n"

        return code + "\n" + main_code

    def _generate_param_declaration(
        self, name: str, param_type: str, value: Any
    ) -> str:
        """Generate C++ parameter declaration with embedded value."""
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
            if "vector<vector" in param_type or "std::vector<std::vector" in param_type:
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
            return f"    // TODO: Initialize {param_type} {name} with value {value}\n"

    def _generate_output(self, cpp_type: str, var_name: str) -> str:
        """Generate output code for a variable of given type."""
        if cpp_type in ["int", "double", "float", "long", "short", "char"]:
            return f"    std::cout << {var_name} << std::endl;\n"
        elif cpp_type == "bool":
            return f'    std::cout << ({var_name} ? "true" : "false") << std::endl;\n'
        elif cpp_type in ["std::string", "string"]:
            return f'    std::cout << "\\"" << {var_name} << "\\"" << std::endl;\n'
        elif "vector" in cpp_type:
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
            return f"    std::cout << {var_name} << std::endl;\n"
