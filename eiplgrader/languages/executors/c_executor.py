"""C language executor for code testing."""

import os
import json
from typing import Dict, Any, List
from .base_executors import CompiledLanguageExecutor


class CExecutor(CompiledLanguageExecutor):
    """Executor for C language code testing."""

    def __init__(self):
        super().__init__(compile_cmd=["gcc"], run_cmd=None, file_ext=".c")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare C code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # Ensure code has necessary headers
        if "#include <stdio.h>" not in code:
            code = "#include <stdio.h>\n" + code
        if "#include <stdlib.h>" not in code:
            code = "#include <stdlib.h>\n" + code
        if "#include <string.h>" not in code:
            code = "#include <string.h>\n" + code

        # Generate main function with test harness
        main_code = """
int main() {
    // Read test parameters from stdin as JSON
    char buffer[4096];
    if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
        fprintf(stderr, "Error reading input\\n");
        return 1;
    }
    
    // Simple JSON parsing for basic types
"""

        # Generate parameter parsing based on test case
        param_names = list(parameters.keys())
        param_types = self._infer_c_types(parameters)

        for i, (name, c_type, value) in enumerate(
            zip(param_names, param_types, parameters.values())
        ):
            if c_type == "int":
                main_code += f"    int {name};\n"
                main_code += f'    sscanf(strstr(buffer, "\\"{name}\\":") + strlen("\\"{name}\\":"), "%d", &{name});\n'
            elif c_type == "double":
                main_code += f"    double {name};\n"
                main_code += f'    sscanf(strstr(buffer, "\\"{name}\\":") + strlen("\\"{name}\\":"), "%lf", &{name});\n'
            elif c_type == "char*":
                main_code += f"    char {name}[256];\n"
                main_code += f'    char* {name}_start = strstr(buffer, "\\"{name}\\": \\"") + strlen("\\"{name}\\": \\"");\n'
                main_code += f"    char* {name}_end = strchr({name}_start, '\\\"');\n"
                main_code += (
                    f"    strncpy({name}, {name}_start, {name}_end - {name}_start);\n"
                )
                main_code += f"    {name}[{name}_end - {name}_start] = '\\0';\n"
            elif c_type == "int*":
                # For arrays, we'll parse them manually
                if isinstance(value, list):
                    array_size = len(value)
                    main_code += f"    int {name}[{array_size}];\n"
                    # Simple array parsing - assume format like [1, 2, 3]
                    main_code += f"    // Parse array {name}\n"
                    main_code += f'    char* {name}_ptr = strstr(buffer, "\\"{name}\\": [") + strlen("\\"{name}\\": [");\n'
                    for j in range(array_size):
                        if j > 0:
                            main_code += (
                                f"    {name}_ptr = strchr({name}_ptr, ',') + 1;\n"
                            )
                        main_code += f'    sscanf({name}_ptr, "%d", &{name}[{j}]);\n'

        # Generate function call based on inplace mode
        if inplace_mode == "0":
            # Normal function call - function returns a value
            # Infer return type from expected value
            expected = test_case.get("expected")
            if isinstance(expected, int):
                return_type = "int"
                format_spec = "%d"
            elif isinstance(expected, float):
                return_type = "double"
                format_spec = "%lf"
            elif isinstance(expected, str):
                return_type = "char*"
                format_spec = '\\"%s\\"'
            elif isinstance(expected, list) and all(
                isinstance(x, int) for x in expected
            ):
                return_type = "int*"
                format_spec = None  # Handle array output separately
            else:
                return_type = "void*"
                format_spec = None

            if return_type == "int*" and isinstance(expected, list):
                # For array returns, we need to handle it specially
                array_size = len(expected)
                main_code += f"""
    // Call the function
    int* result = {function_name}({', '.join(param_names)});
    
    // Output result as JSON array
    printf("[");
    for (int i = 0; i < {array_size}; i++) {{
        if (i > 0) printf(", ");
        printf("%d", result[i]);
    }}
    printf("]\\n");
"""
            elif format_spec:
                main_code += f"""
    // Call the function
    {return_type} result = {function_name}({', '.join(param_names)});
    
    // Output result as JSON
    printf("{format_spec}\\n", result);
"""
            else:
                # For other types, just print null
                main_code += f"""
    // Call the function
    {function_name}({', '.join(param_names)});
    printf("null\\n");
"""
        elif inplace_mode == "1":
            # Function modifies arguments in-place
            if param_names:
                first_param = param_names[0]
                first_type = param_types[0]
                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )

                if first_type == "int*" and isinstance(parameters[first_param], list):
                    array_size = len(parameters[first_param])
                    if other_params:
                        main_code += f"""
    // Call the function (modifies first parameter)
    {function_name}({first_param}, {other_params});
    
    // Output modified array as JSON
    printf("[");
    for (int i = 0; i < {array_size}; i++) {{
        if (i > 0) printf(", ");
        printf("%d", {first_param}[i]);
    }}
    printf("]\\n");
"""
                    else:
                        main_code += f"""
    // Call the function (modifies parameter)
    {function_name}({first_param});
    
    // Output modified array as JSON
    printf("[");
    for (int i = 0; i < {array_size}; i++) {{
        if (i > 0) printf(", ");
        printf("%d", {first_param}[i]);
    }}
    printf("]\\n");
"""
                else:
                    # For non-array in-place modifications
                    # Check if we have multiple parameters and if they might need to be passed by reference too
                    if other_params:
                        # Special handling for swap-like functions
                        if len(param_names) == 2 and all(
                            param_types[i] in ["int", "double"] for i in range(2)
                        ):
                            # Both parameters might need to be passed by reference
                            main_code += f"""
    // Call the function (modifies both parameters)
    {function_name}(&{param_names[0]}, &{param_names[1]});
    
    // Output modified first parameter as JSON
    printf("%d\\n", {first_param});
"""
                        else:
                            main_code += f"""
    // Call the function (modifies first parameter)
    {function_name}(&{first_param}, {other_params});
    
    // Output modified parameter as JSON
    printf("%d\\n", {first_param});
"""
                    else:
                        main_code += f"""
    // Call the function (modifies parameter)
    {function_name}(&{first_param});
    
    // Output modified parameter as JSON
    printf("%d\\n", {first_param});
"""
            else:
                main_code += f"""
    // Call the function
    {function_name}();
    printf("null\\n");
"""
        elif inplace_mode == "2":
            # Function both modifies in-place and returns a value
            if param_names:
                first_param = param_names[0]
                first_type = param_types[0]
                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )

                # For simplicity, assume it returns an int
                if first_type == "int*":
                    if other_params:
                        main_code += f"""
    // Call the function (modifies first parameter and returns value)
    int result = {function_name}({first_param}, {other_params});
    
    // Output return value
    printf("%d\\n", result);
"""
                    else:
                        main_code += f"""
    // Call the function
    int result = {function_name}({first_param});
    
    // Output return value
    printf("%d\\n", result);
"""
                else:
                    if other_params:
                        main_code += f"""
    // Call the function (modifies first parameter and returns value)
    int result = {function_name}(&{first_param}, {other_params});
    
    // Output return value
    printf("%d\\n", result);
"""
                    else:
                        main_code += f"""
    // Call the function
    int result = {function_name}(&{first_param});
    
    // Output return value
    printf("%d\\n", result);
"""
            else:
                main_code += f"""
    // Call the function
    int result = {function_name}();
    printf("%d\\n", result);
"""

        main_code += """
    return 0;
}
"""

        # Combine everything
        return code + "\n" + main_code

    def _infer_c_types(self, parameters: Dict[str, Any]) -> List[str]:
        """Infer C types from parameter values."""
        types = []
        for value in parameters.values():
            if isinstance(value, bool):
                types.append("int")  # C uses int for bool
            elif isinstance(value, int):
                types.append("int")
            elif isinstance(value, float):
                types.append("double")
            elif isinstance(value, str):
                types.append("char*")
            elif isinstance(value, list):
                if not value:
                    types.append("int*")  # Default to int array
                elif isinstance(value[0], int):
                    types.append("int*")
                elif isinstance(value[0], float):
                    types.append("double*")
                else:
                    types.append("void*")
            else:
                types.append("void*")
        return types
