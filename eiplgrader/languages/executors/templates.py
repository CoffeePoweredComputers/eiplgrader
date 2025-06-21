"""
Template configurations for all supported language executors.

This module provides reusable template configurations and functions for generating
test harnesses, parameter declarations, and output formatting across different
programming languages. Templates use the string_utils module for proper formatting
and indentation.
"""

import json
from typing import Any, Dict, List, Optional, Union
from .string_utils import (
    CodeBuilder,
    dedent_template,
    escape_string_literal,
    build_parameter_list,
)


class TemplateConfig:
    """Base configuration class for language-specific templates."""

    def __init__(self, language: str):
        self.language = language

    def get_imports_template(self) -> str:
        """Get template for import/include statements."""
        raise NotImplementedError

    def get_parameter_declaration_template(self) -> str:
        """Get template for parameter declarations."""
        raise NotImplementedError

    def get_function_call_template(self) -> str:
        """Get template for function calls."""
        raise NotImplementedError

    def get_output_template(self) -> str:
        """Get template for output formatting."""
        raise NotImplementedError

    def get_main_harness_template(self) -> str:
        """Get main test harness template."""
        raise NotImplementedError


class JavaTemplateConfig(TemplateConfig):
    """Template configuration for Java language."""

    def __init__(self):
        super().__init__("java")

    def get_imports_template(self) -> str:
        return dedent_template(
            """
            import java.util.*;
            {additional_imports}
        """
        ).strip()

    def get_parameter_declaration_template(self) -> str:
        return dedent_template(
            """
            // {param_name}: {param_type}
            {param_type} {param_name} = {param_value};
        """
        ).strip()

    def get_function_call_template(self) -> str:
        return "Solution.{function_name}({param_list})"

    def get_output_template(self) -> str:
        return dedent_template(
            """
            // Output handling for {data_type}
            {output_code}
        """
        ).strip()

    def get_main_harness_template(self) -> str:
        return dedent_template(
            """
            {imports}
            
            class Solution {{
            {method_code}
            }}
            
            public class Test {{
                public static void main(String[] args) {{
                    // Test parameters
            {param_declarations}
                    
                    // Call function and handle result
            {result_handling}
                }}
            }}
        """
        ).strip()


class GoTemplateConfig(TemplateConfig):
    """Template configuration for Go language."""

    def __init__(self):
        super().__init__("go")

    def get_imports_template(self) -> str:
        return dedent_template(
            """
            import (
            {import_list}
            )
        """
        ).strip()

    def get_parameter_declaration_template(self) -> str:
        return "    {param_name} := {param_value}"

    def get_function_call_template(self) -> str:
        return "{function_name}({param_list})"

    def get_output_template(self) -> str:
        return dedent_template(
            """
            // Output {data_type}
            {output_code}
        """
        ).strip()

    def get_main_harness_template(self) -> str:
        return dedent_template(
            """
            package main
            
            {imports}
            
            {user_code}
            
            func main() {{
            {param_declarations}

            {result_handling}
            }}
        """
        ).strip()


class CppTemplateConfig(TemplateConfig):
    """Template configuration for C++ language."""

    def __init__(self):
        super().__init__("cpp")

    def get_imports_template(self) -> str:
        return dedent_template(
            """
            #include <iostream>
            #include <vector>
            #include <string>
            #include <algorithm>
            {additional_includes}
        """
        ).strip()

    def get_parameter_declaration_template(self) -> str:
        return "    {param_type} {param_name} = {param_value};"

    def get_function_call_template(self) -> str:
        return "{function_name}({param_list})"

    def get_output_template(self) -> str:
        return dedent_template(
            """
            // Output {data_type}
            {output_code}
        """
        ).strip()

    def get_main_harness_template(self) -> str:
        return dedent_template(
            """
            {includes}
            
            {user_code}
            
            int main() {{
                // Test case values
            {param_declarations}

            {result_handling}
                return 0;
            }}
        """
        ).strip()


class CTemplateConfig(TemplateConfig):
    """Template configuration for C language."""

    def __init__(self):
        super().__init__("c")

    def get_imports_template(self) -> str:
        return dedent_template(
            """
            #include <stdio.h>
            #include <stdlib.h>
            #include <string.h>
            {additional_includes}
        """
        ).strip()

    def get_parameter_declaration_template(self) -> str:
        return "    {param_type} {param_name} = {param_value};"

    def get_function_call_template(self) -> str:
        return "{function_name}({param_list})"

    def get_output_template(self) -> str:
        return dedent_template(
            """
            // Output {data_type}
            {output_code}
        """
        ).strip()

    def get_main_harness_template(self) -> str:
        return dedent_template(
            """
            {includes}
            
            {user_code}
            
            int main() {{
                // Test case values
            {param_declarations}

            {result_handling}
                return 0;
            }}
        """
        ).strip()


class HaskellTemplateConfig(TemplateConfig):
    """Template configuration for Haskell language."""

    def __init__(self):
        super().__init__("haskell")

    def get_imports_template(self) -> str:
        return dedent_template(
            """
            import Data.List
            import Text.Read
            {additional_imports}
        """
        ).strip()

    def get_parameter_declaration_template(self) -> str:
        return "let {param_name} = {param_value}"

    def get_function_call_template(self) -> str:
        return "{function_name} {param_list}"

    def get_output_template(self) -> str:
        return dedent_template(
            """
            -- Output {data_type}
            {output_code}
        """
        ).strip()

    def get_main_harness_template(self) -> str:
        return dedent_template(
            """
            {imports}
            
            {user_code}
            
            main :: IO ()
            main = do
            {param_declarations}
            {result_handling}
        """
        ).strip()


class JavaScriptTemplateConfig(TemplateConfig):
    """Template configuration for JavaScript language."""

    def __init__(self):
        super().__init__("javascript")

    def get_imports_template(self) -> str:
        return "// JavaScript imports would go here if needed"

    def get_parameter_declaration_template(self) -> str:
        return "const {param_name} = {param_value};"

    def get_function_call_template(self) -> str:
        return "await {function_name}({param_list})"

    def get_output_template(self) -> str:
        return dedent_template(
            """
            // Output handling for {data_type}
            console.log(JSON.stringify({output_value}));
        """
        ).strip()

    def get_main_harness_template(self) -> str:
        return dedent_template(
            """
            // Student code
            {user_code}
            
            // Test harness
            (async function runTest() {{
                try {{
                    // Get test parameters
                    const testParams = {test_params};
                    const paramTypes = {param_types};
                    const expectedType = {expected_type};
                    
            {param_setup}
                    
                    // Check if function exists
                    if (typeof {function_name} === 'undefined') {{
                        console.error(JSON.stringify({{
                            error: "Function '{function_name}' is not defined"
                        }}));
                        process.exit(1);
                    }}
                    
            {result_handling}
                    
                }} catch (error) {{
                    console.error(JSON.stringify({{
                        error: error.message || String(error)
                    }}));
                    process.exit(1);
                }}
            }})();
        """
        ).strip()


# Template registry
TEMPLATE_CONFIGS = {
    "java": JavaTemplateConfig(),
    "go": GoTemplateConfig(),
    "cpp": CppTemplateConfig(),
    "c": CTemplateConfig(),
    "haskell": HaskellTemplateConfig(),
    "javascript": JavaScriptTemplateConfig(),
}


def get_template_config(language: str) -> TemplateConfig:
    """Get template configuration for a specific language."""
    if language not in TEMPLATE_CONFIGS:
        raise ValueError(f"No template configuration found for language: {language}")
    return TEMPLATE_CONFIGS[language]


# Template generation functions
def generate_java_param_declaration(
    param_name: str, param_type: str, param_value: Any
) -> str:
    """Generate Java parameter declaration with embedded value."""
    builder = CodeBuilder()

    if param_type == "String":
        escaped_value = escape_string_literal(str(param_value))
        builder.add_line(f"String {param_name} = {escaped_value};")
    elif param_type == "boolean":
        builder.add_line(f"boolean {param_name} = {str(param_value).lower()};")
    elif param_type in ["int", "long", "double", "float"]:
        builder.add_line(f"{param_type} {param_name} = {param_value};")
    elif param_type == "int[]":
        values = ", ".join(str(v) for v in param_value)
        builder.add_line(f"int[] {param_name} = {{{values}}};")
    elif param_type == "double[]":
        values = ", ".join(str(v) for v in param_value)
        builder.add_line(f"double[] {param_name} = {{{values}}};")
    elif param_type == "String[]":
        values = ", ".join(escape_string_literal(str(v)) for v in param_value)
        builder.add_line(f"String[] {param_name} = {{{values}}};")
    elif param_type == "List<Integer>":
        values = ", ".join(str(v) for v in param_value)
        builder.add_line(f"List<Integer> {param_name} = Arrays.asList({values});")
    elif param_type == "List<String>":
        values = ", ".join(escape_string_literal(str(v)) for v in param_value)
        builder.add_line(f"List<String> {param_name} = Arrays.asList({values});")
    else:
        builder.add_line(f"// Unsupported type {param_type} for {param_name}")

    return builder.build()



def generate_java_output(
    data_type: str, value_expr: str, is_direct_call: bool = False
) -> str:
    """Generate Java output formatting code."""
    builder = CodeBuilder()

    if is_direct_call:
        # For direct function calls, assign to result variable first
        if data_type == "int":
            builder.add_line(f"int result = {value_expr};")
            builder.add_line("System.out.println(result);")
        elif data_type == "double":
            builder.add_line(f"double result = {value_expr};")
            builder.add_line("System.out.println(result);")
        elif data_type == "boolean":
            builder.add_line(f"boolean result = {value_expr};")
            builder.add_line("System.out.println(result);")
        elif data_type == "String":
            builder.add_line(f"String result = {value_expr};")
            builder.add_line('System.out.println("\\"" + result + "\\"");')
        elif data_type == "int[]":
            builder.add_line(f"int[] result = {value_expr};")
            builder.add_line('System.out.print("[");')
            builder.add_line("for (int i = 0; i < result.length; i++) {")
            with builder.indent():
                builder.add_line('if (i > 0) System.out.print(",");')
                builder.add_line("System.out.print(result[i]);")
            builder.add_line("}")
            builder.add_line('System.out.println("]");')
        elif data_type == "double[]":
            builder.add_line(f"double[] result = {value_expr};")
            builder.add_line('System.out.print("[");')
            builder.add_line("for (int i = 0; i < result.length; i++) {")
            with builder.indent():
                builder.add_line('if (i > 0) System.out.print(",");')
                builder.add_line("System.out.print(result[i]);")
            builder.add_line("}")
            builder.add_line('System.out.println("]");')
        elif data_type == "String[]":
            builder.add_line(f"String[] result = {value_expr};")
            builder.add_line('System.out.print("[");')
            builder.add_line("for (int i = 0; i < result.length; i++) {")
            with builder.indent():
                builder.add_line('if (i > 0) System.out.print(",");')
                builder.add_line('System.out.print("\\"" + result[i] + "\\"");')
            builder.add_line("}")
            builder.add_line('System.out.println("]");')
        else:
            builder.add_line(f"Object result = {value_expr};")
            builder.add_line("System.out.println(result);")
    else:
        # Direct output of variable
        if data_type in ("int", "double", "boolean"):
            builder.add_line(f"System.out.println({value_expr});")
        elif data_type == "String":
            builder.add_line(f'System.out.println("\\"" + {value_expr} + "\\"");')
        elif data_type == "int[]":
            builder.add_line('System.out.print("[");')
            builder.add_line(f"for (int i = 0; i < {value_expr}.length; i++) {{")
            with builder.indent():
                builder.add_line('if (i > 0) System.out.print(",");')
                builder.add_line(f"System.out.print({value_expr}[i]);")
            builder.add_line("}")
            builder.add_line('System.out.println("]");')
        elif data_type == "double[]":
            builder.add_line('System.out.print("[");')
            builder.add_line(f"for (int i = 0; i < {value_expr}.length; i++) {{")
            with builder.indent():
                builder.add_line('if (i > 0) System.out.print(",");')
                builder.add_line(f"System.out.print({value_expr}[i]);")
            builder.add_line("}")
            builder.add_line('System.out.println("]");')
        elif data_type == "String[]":
            builder.add_line('System.out.print("[");')
            builder.add_line(f"for (int i = 0; i < {value_expr}.length; i++) {{")
            with builder.indent():
                builder.add_line('if (i > 0) System.out.print(",");')
                builder.add_line(f'System.out.print("\\"" + {value_expr}[i] + "\\"");')
            builder.add_line("}")
            builder.add_line('System.out.println("]");')
        else:
            builder.add_line(f"System.out.println({value_expr});")

    return builder.build()



def generate_go_param_declaration(
    param_name: str, param_type: str, param_value: Any
) -> str:
    """Generate Go parameter declaration with embedded value."""
    if param_type == "string":
        escaped_value = str(param_value).replace("\\", "\\\\").replace('"', '\\"')
        return f'    {param_name} := "{escaped_value}"'
    elif param_type == "bool":
        return f"    {param_name} := {str(param_value).lower()}"
    elif param_type in ["int", "float64"]:
        return f"    {param_name} := {param_value}"
    elif param_type == "[]int":
        values = ", ".join(str(v) for v in param_value)
        return f"    {param_name} := []int{{{values}}}"
    elif param_type == "[]string":
        values = ", ".join(
            f'"{str(v).replace("\\", "\\\\").replace('"', '\\"')}"' for v in param_value
        )
        return f"    {param_name} := []string{{{values}}}"
    elif param_type == "[]float64":
        values = ", ".join(str(v) for v in param_value)
        return f"    {param_name} := []float64{{{values}}}"
    elif param_type == "[]bool":
        values = ", ".join(str(v).lower() for v in param_value)
        return f"    {param_name} := []bool{{{values}}}"
    else:
        return f"    // Unsupported type {param_type} for {param_name}"


def generate_go_output(data_type: str, value_expr: str) -> str:
    """Generate Go output formatting code."""
    builder = CodeBuilder()

    if data_type in ["int", "float64", "string", "bool"]:
        builder.add_line(f'fmt.Printf("%v\\n", {value_expr})')
    elif data_type in [
        "[]int",
        "[]string",
        "[]float64",
        "[]bool",
    ] or data_type.startswith("map"):
        builder.add_line(f"jsonBytes, _ := json.Marshal({value_expr})")
        builder.add_line("os.Stdout.Write(jsonBytes)")
        builder.add_line('os.Stdout.WriteString("\\n")')
    else:
        builder.add_line(f'fmt.Printf("%v\\n", {value_expr})')

    return builder.build()


def generate_cpp_param_declaration(
    param_name: str, param_type: str, param_value: Any
) -> str:
    """Generate C++ parameter declaration with embedded value."""
    if param_type == "std::string":
        escaped_value = escape_string_literal(str(param_value))
        return f"    std::string {param_name} = {escaped_value};"
    elif param_type == "bool":
        return f"    bool {param_name} = {str(param_value).lower()};"
    elif param_type in ["int", "double", "float", "long"]:
        return f"    {param_type} {param_name} = {param_value};"
    elif param_type == "std::vector<int>":
        values = ", ".join(str(v) for v in param_value)
        return f"    std::vector<int> {param_name} = {{{values}}};"
    elif param_type == "std::vector<std::string>":
        values = ", ".join(escape_string_literal(str(v)) for v in param_value)
        return f"    std::vector<std::string> {param_name} = {{{values}}};"
    elif param_type == "std::vector<double>":
        values = ", ".join(str(v) for v in param_value)
        return f"    std::vector<double> {param_name} = {{{values}}};"
    else:
        return f"    // Unsupported type {param_type} for {param_name}"


def generate_cpp_output(data_type: str, value_expr: str) -> str:
    """Generate C++ output formatting code."""
    builder = CodeBuilder()

    if data_type in ["int", "double", "float", "long", "bool"]:
        builder.add_line(f"std::cout << {value_expr} << std::endl;")
    elif data_type == "std::string":
        builder.add_line(f"std::cout << {value_expr} << std::endl;")
    elif data_type.startswith("std::vector<"):
        builder.add_line(f'std::cout << "[";')
        builder.add_line(f"for (size_t i = 0; i < {value_expr}.size(); ++i) {{")
        with builder.indent():
            builder.add_line(f'if (i > 0) std::cout << ", ";')
            if "std::string" in data_type:
                builder.add_line(f'std::cout << "\\"" << {value_expr}[i] << "\\"";')
            else:
                builder.add_line(f"std::cout << {value_expr}[i];")
        builder.add_line("}")
        builder.add_line('std::cout << "]" << std::endl;')
    else:
        builder.add_line(f"std::cout << {value_expr} << std::endl;")

    return builder.build()


def generate_c_param_declaration(
    param_name: str, param_type: str | None, param_value: Any
) -> str:
    """Generate C parameter declaration with embedded value."""
    if param_type is None:
        return f"    // Unknown type for {param_name}"
    elif param_type == "int":
        return f"    int {param_name} = {param_value};"
    elif param_type == "double":
        return f"    double {param_name} = {param_value};"
    elif param_type == "char*":
        return f'    char {param_name}[] = "{param_value}";'
    elif param_type == "int*" and isinstance(param_value, list):
        values_str = ", ".join(str(v) for v in param_value)
        return f"    int {param_name}[] = {{{values_str}}};"
    elif param_type == "double*" and isinstance(param_value, list):
        values_str = ", ".join(str(v) for v in param_value)
        return f"    double {param_name}[] = {{{values_str}}};"
    else:
        return f"    // Unsupported type: {param_type} {param_name}"


def generate_c_output(data_type: str, value_expr: str) -> str:
    """Generate C output formatting code."""
    # Get the format specifier like the original code
    format_specs = {
        "int": "%d",
        "double": "%.6f",
        "float": "%.6f",
        "char*": '\\"%s\\"',
    }
    format_spec = format_specs.get(data_type, "%d")
    
    return f'printf("{format_spec}\\n", {value_expr});'


def generate_haskell_param_declaration(
    param_name: str, param_type: str, param_value: Any
) -> str:
    """Generate Haskell parameter declaration with embedded value."""
    builder = CodeBuilder()

    if param_type == "Int":
        builder.add_line(f"let {param_name} = {param_value} :: Int")
    elif param_type == "String":
        escaped_value = escape_string_literal(str(param_value))
        builder.add_line(f"let {param_name} = {escaped_value} :: String")
    elif param_type == "Bool":
        haskell_bool = "True" if param_value else "False"
        builder.add_line(f"let {param_name} = {haskell_bool} :: Bool")
    elif param_type == "Double":
        builder.add_line(f"let {param_name} = {param_value} :: Double")
    elif param_type == "[Int]" and isinstance(param_value, list):
        values = ", ".join(str(v) for v in param_value)
        builder.add_line(f"let {param_name} = [{values}] :: [Int]")
    elif param_type == "[Double]" and isinstance(param_value, list):
        values = ", ".join(str(v) for v in param_value)
        builder.add_line(f"let {param_name} = [{values}] :: [Double]")
    elif param_type == "[String]" and isinstance(param_value, list):
        escaped_values = []
        for v in param_value:
            escaped_values.append(escape_string_literal(str(v)))
        values_str = ", ".join(escaped_values)
        builder.add_line(f"let {param_name} = [{values_str}] :: [String]")
    else:
        builder.add_line(f"let {param_name} = {param_value} :: {param_type}")

    return builder.build()



def generate_haskell_output(data_type: str, value_expr: str) -> str:
    """Generate Haskell output formatting code."""
    if data_type == "Bool":
        return f'putStrLn $ if {value_expr} then "true" else "false"'
    elif data_type in ["Int", "Double"]:
        return f"print {value_expr}"
    elif data_type == "String":
        return f'putStrLn $ "\\"" ++ {value_expr} ++ "\\""'
    elif data_type and data_type.startswith("["):
        return f"print {value_expr}"
    else:
        return f"print {value_expr}"



def generate_inplace_function_call(
    language: str,
    function_name: str,
    param_names: List[str],
    inplace_mode: str,
    param_types: Optional[Dict[str, str]] = None,
    expected_type: Optional[str] = None,
) -> str:
    """Generate function call code based on inplace mode."""
    builder = CodeBuilder()

    if inplace_mode == "0":
        # Normal function call with return value
        if language == "java":
            if param_names:
                builder.add_line(
                    f"{expected_type} result = Solution.{function_name}({', '.join(param_names)});"
                )
            else:
                builder.add_line(
                    f"{expected_type} result = Solution.{function_name}();"
                )
            builder.add_lines(
                generate_java_output(expected_type, "result", is_direct_call=True)
            )
        elif language == "go":
            if param_names:
                builder.add_line(f"result := {function_name}({', '.join(param_names)})")
            else:
                builder.add_line(f"result := {function_name}()")
            builder.add_lines(generate_go_output(expected_type, "result"))
        elif language == "cpp":
            if param_names:
                builder.add_line(
                    f"{expected_type} result = {function_name}({', '.join(param_names)});"
                )
            else:
                builder.add_line(f"{expected_type} result = {function_name}();")
            builder.add_lines(generate_cpp_output(expected_type, "result"))
        elif language == "c":
            if param_names:
                builder.add_line(
                    f"{expected_type} result = {function_name}({', '.join(param_names)});"
                )
            else:
                builder.add_line(f"{expected_type} result = {function_name}();")
            builder.add_lines(generate_c_output(expected_type, "result"))
        elif language == "haskell":
            if param_names:
                builder.add_line(
                    f"let result = {function_name} {' '.join(param_names)}"
                )
            else:
                builder.add_line(f"let result = {function_name}")
            builder.add_line(generate_haskell_output(expected_type, "result"))
        elif language == "javascript":
            builder.add_line("let result;")
            if param_names:
                builder.add_line(f"result = await {function_name}(...args);")
            else:
                builder.add_line(f"result = await {function_name}();")
            builder.add_line("console.log(JSON.stringify(result));")

    elif inplace_mode == "1":
        # In-place modification
        if language == "java":
            if param_names:
                builder.add_line(f"Solution.{function_name}({', '.join(param_names)});")
                first_param = param_names[0]
                first_type = param_types.get(first_param) if param_types else "Object"
                builder.add_lines(generate_java_output(first_type, first_param))
            else:
                builder.add_line(f"Solution.{function_name}();")
                builder.add_line('System.out.println("null");')
        elif language == "go":
            if param_names:
                # Handle slice references
                args = []
                for p in param_names:
                    if param_types and param_types[p].startswith("[]"):
                        args.append(p)  # Slices are already references
                    else:
                        args.append("&" + p)  # Other types need address-of
                builder.add_line(f"{function_name}({', '.join(args)})")
                first_param = param_names[0]
                first_type = (
                    param_types.get(first_param) if param_types else "interface{}"
                )
                builder.add_lines(generate_go_output(first_type, first_param))
            else:
                builder.add_line(f"{function_name}()")
                builder.add_line('fmt.Println("null")')
        elif language == "cpp":
            if param_names:
                builder.add_line(f"{function_name}({', '.join(param_names)});")
                first_param = param_names[0]
                first_type = param_types.get(first_param) if param_types else "int"
                builder.add_lines(generate_cpp_output(first_type, first_param))
            else:
                builder.add_line(f"{function_name}();")
                builder.add_line('std::cout << "null" << std::endl;')
        elif language == "javascript":
            builder.add_line("if (args.length > 0) {")
            with builder.indent():
                builder.add_line("// Deep copy first argument")
                builder.add_line("let firstArg = JSON.parse(JSON.stringify(args[0]));")
                builder.add_line(f"await {function_name}(firstArg, ...args.slice(1));")
                builder.add_line("result = firstArg;")
            builder.add_line("} else {")
            with builder.indent():
                builder.add_line(f"result = await {function_name}();")
            builder.add_line("}")
            builder.add_line("console.log(JSON.stringify(result));")

    elif inplace_mode == "2":
        # Both modify and return
        if language == "java":
            if param_names:
                builder.add_line(
                    f"{expected_type} result = Solution.{function_name}({', '.join(param_names)});"
                )
            else:
                builder.add_line(
                    f"{expected_type} result = Solution.{function_name}();"
                )
            builder.add_lines(
                generate_java_output(expected_type, "result", is_direct_call=True)
            )
        elif language == "go":
            if param_names:
                args = []
                for p in param_names:
                    if param_types and param_types[p].startswith("[]"):
                        args.append(p)
                    else:
                        args.append("&" + p)
                builder.add_line(f"result := {function_name}({', '.join(args)})")
            else:
                builder.add_line(f"result := {function_name}()")
            builder.add_lines(generate_go_output(expected_type, "result"))
        elif language == "cpp":
            if param_names:
                builder.add_line(
                    f"{expected_type} result = {function_name}({', '.join(param_names)});"
                )
            else:
                builder.add_line(f"{expected_type} result = {function_name}();")
            builder.add_lines(generate_cpp_output(expected_type, "result"))
        elif language == "javascript":
            builder.add_line("if (args.length > 0) {")
            with builder.indent():
                builder.add_line("// Deep copy first argument")
                builder.add_line("let firstArg = JSON.parse(JSON.stringify(args[0]));")
                builder.add_line(
                    f"let returnValue = await {function_name}(firstArg, ...args.slice(1));"
                )
                builder.add_line(
                    "// Use return value if it exists, otherwise use modified argument"
                )
                builder.add_line(
                    "result = returnValue !== undefined ? returnValue : firstArg;"
                )
            builder.add_line("} else {")
            with builder.indent():
                builder.add_line(f"result = await {function_name}();")
            builder.add_line("}")
            builder.add_line("console.log(JSON.stringify(result));")

    return builder.build()


# Language-specific template functions
PARAM_DECLARATION_GENERATORS = {
    "java": generate_java_param_declaration,
    "go": generate_go_param_declaration,
    "cpp": generate_cpp_param_declaration,
    "c": generate_c_param_declaration,
    "haskell": generate_haskell_param_declaration,
}

OUTPUT_GENERATORS = {
    "java": generate_java_output,
    "go": generate_go_output,
    "cpp": generate_cpp_output,
    "c": generate_c_output,
    "haskell": generate_haskell_output,
}


def generate_javascript_parameter_setup(parameters: Dict[str, Any], parameter_types: Dict[str, str], expected_type: str) -> str:
    """Generate JavaScript parameter setup code."""
    from .string_utils import CodeBuilder
    import json
    
    builder = CodeBuilder()
    
    # Add parameter setup
    builder.add_line("// Get test parameters from command line or hardcoded")
    builder.add_line(f"const testParams = {json.dumps(parameters)};")
    builder.add_line(f"const paramTypes = {json.dumps(parameter_types)};")
    builder.add_line(f"const expectedType = {json.dumps(expected_type)};")
    builder.add_line()
    
    # Add parameter validation
    builder.add_line("// Validate parameter types match expected format")
    builder.add_line("const paramNames = Object.keys(testParams);")
    builder.add_line("const args = [];")
    builder.add_line()
    
    builder.add_line("for (const paramName of paramNames) {")
    with builder.indent():
        builder.add_line("if (!paramTypes[paramName]) {")
        with builder.indent():
            builder.add_line("console.error(JSON.stringify({")
            with builder.indent():
                builder.add_line("error: `Missing type for parameter: ${paramName}`")
            builder.add_line("}));")
            builder.add_line("process.exit(1);")
        builder.add_line("}")
        builder.add_line("args.push(testParams[paramName]);")
    builder.add_line("}")
    
    return builder.build()


def generate_javascript_function_check(function_name: str) -> str:
    """Generate JavaScript function existence check."""
    from .string_utils import CodeBuilder
    
    builder = CodeBuilder()
    
    builder.add_line("// Check if function exists")
    builder.add_line(f"if (typeof {function_name} === 'undefined') {{")
    with builder.indent():
        builder.add_line("console.error(JSON.stringify({")
        with builder.indent():
            builder.add_line(f'error: "Function \'{function_name}\' is not defined"')
        builder.add_line("}));")
        builder.add_line("process.exit(1);")
    builder.add_line("}")
    
    return builder.build()


def generate_javascript_error_handler() -> str:
    """Generate JavaScript error handling code."""
    from .string_utils import CodeBuilder
    
    builder = CodeBuilder()
    
    builder.add_line("} catch (error) {")
    with builder.indent():
        builder.add_line("console.error(JSON.stringify({")
        with builder.indent():
            builder.add_line("error: error.message || String(error)")
        builder.add_line("}));")
        builder.add_line("process.exit(1);")
    builder.add_line("}")
    
    return builder.build()


def generate_javascript_async_wrapper(inner_code: str) -> str:
    """Generate JavaScript async wrapper function."""
    from .string_utils import CodeBuilder
    
    builder = CodeBuilder()
    
    builder.add_line("(async function runTest() {")
    with builder.indent():
        builder.add_line("try {")
        with builder.indent():
            builder.add_lines(inner_code)
        builder.add_lines(generate_javascript_error_handler())
    builder.add_line("})();")
    
    return builder.build()





def get_param_declaration_generator(language: str):
    """Get parameter declaration generator function for a language."""
    if language not in PARAM_DECLARATION_GENERATORS:
        raise ValueError(f"No parameter declaration generator for language: {language}")
    return PARAM_DECLARATION_GENERATORS[language]


def get_output_generator(language: str):
    """Get output generator function for a language."""
    if language not in OUTPUT_GENERATORS:
        raise ValueError(f"No output generator for language: {language}")
    return OUTPUT_GENERATORS[language]
