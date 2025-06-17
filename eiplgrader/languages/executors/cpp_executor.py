"""C++ language executor for code testing."""

import os
import json
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
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # Add JSON parsing capability using simple string parsing
        # Since C++ doesn't have built-in JSON, we'll use a simple approach
        json_parser = """
// Simple JSON-like parsing helpers
namespace json_parse {
    std::vector<int> parse_int_array(const std::string& str) {
        std::vector<int> result;
        std::stringstream ss(str);
        char ch;
        int num;
        
        // Skip opening bracket
        ss >> ch;
        if (ch != '[') return result;
        
        // Parse numbers
        while (ss >> num) {
            result.push_back(num);
            ss >> ch; // comma or closing bracket
            if (ch == ']') break;
        }
        
        return result;
    }
    
    std::vector<std::string> parse_string_array(const std::string& str) {
        std::vector<std::string> result;
        size_t pos = 1; // Skip opening bracket
        size_t end = str.find_last_of(']');
        
        while (pos < end) {
            // Skip whitespace and quotes
            while (pos < end && (str[pos] == ' ' || str[pos] == '"' || str[pos] == ',')) pos++;
            
            if (pos >= end) break;
            
            // Find end of string
            size_t start = pos;
            if (str[pos] == '"') {
                start++;
                pos = str.find('"', start);
            } else {
                while (pos < end && str[pos] != ',' && str[pos] != ']') pos++;
            }
            
            if (start < pos) {
                result.push_back(str.substr(start, pos - start));
            }
            pos++;
        }
        
        return result;
    }
    
    std::map<std::string, std::string> parse_object(const std::string& json) {
        std::map<std::string, std::string> result;
        size_t pos = 1; // Skip opening brace
        
        while (pos < json.length() - 1) {
            // Skip whitespace
            while (pos < json.length() && (json[pos] == ' ' || json[pos] == ',')) pos++;
            
            // Find key
            size_t keyStart = json.find('"', pos);
            if (keyStart == std::string::npos) break;
            size_t keyEnd = json.find('"', keyStart + 1);
            if (keyEnd == std::string::npos) break;
            
            std::string key = json.substr(keyStart + 1, keyEnd - keyStart - 1);
            
            // Find value
            size_t colonPos = json.find(':', keyEnd);
            if (colonPos == std::string::npos) break;
            
            pos = colonPos + 1;
            while (pos < json.length() && json[pos] == ' ') pos++;
            
            size_t valueStart = pos;
            size_t valueEnd = valueStart;
            
            if (json[pos] == '"') {
                // String value
                valueStart++;
                valueEnd = json.find('"', valueStart);
                if (valueEnd != std::string::npos) {
                    result[key] = json.substr(valueStart, valueEnd - valueStart);
                    pos = valueEnd + 1; // Move past the closing quote
                } else {
                    break;
                }
            } else if (json[pos] == '[') {
                // Array value
                int bracketCount = 1;
                valueEnd = pos + 1;
                while (valueEnd < json.length() && bracketCount > 0) {
                    if (json[valueEnd] == '[') bracketCount++;
                    else if (json[valueEnd] == ']') bracketCount--;
                    valueEnd++;
                }
                result[key] = json.substr(valueStart, valueEnd - valueStart);
                pos = valueEnd;
            } else {
                // Number or boolean
                while (valueEnd < json.length() && json[valueEnd] != ',' && json[valueEnd] != '}') {
                    valueEnd++;
                }
                result[key] = json.substr(valueStart, valueEnd - valueStart);
                pos = valueEnd;
            }
        }
        
        return result;
    }
}

// Helper to convert various types to JSON string
template<typename T>
std::string to_json(const T& value) {
    std::stringstream ss;
    ss << value;
    return ss.str();
}

template<>
std::string to_json(const std::string& value) {
    return '"' + value + '"';
}

template<>
std::string to_json(const bool& value) {
    return value ? "true" : "false";
}

template<typename T>
std::string to_json(const std::vector<T>& vec) {
    std::stringstream ss;
    ss << "[";
    for (size_t i = 0; i < vec.size(); ++i) {
        if (i > 0) ss << ",";
        ss << to_json(vec[i]);
    }
    ss << "]";
    return ss.str();
}
"""

        # Collect all necessary includes
        necessary_includes = [
            "#include <iostream>",
            "#include <vector>",
            "#include <string>",
            "#include <sstream>",
            "#include <algorithm>",
            "#include <map>",
            "#include <cstdlib>",
        ]

        # Find existing includes in the code
        existing_includes = set()
        lines = code.split("\n")
        include_section_end = 0

        for i, line in enumerate(lines):
            if line.strip().startswith("#include"):
                existing_includes.add(line.strip())
                include_section_end = i + 1
            elif (
                include_section_end > 0
                and line.strip()
                and not line.strip().startswith("#")
            ):
                break

        # Add missing includes
        includes_to_add = []
        for inc in necessary_includes:
            if inc not in existing_includes:
                includes_to_add.append(inc)

        # Rebuild code with all includes and JSON parser
        if include_section_end > 0:
            # Insert missing includes after existing ones
            new_lines = lines[:include_section_end]
            new_lines.extend(includes_to_add)
            new_lines.append("")  # Empty line
            new_lines.append(json_parser)
            new_lines.extend(lines[include_section_end:])
            code = "\n".join(new_lines)
        else:
            # No existing includes, add all at the beginning
            all_includes = "\n".join(necessary_includes)
            code = all_includes + "\n\n" + json_parser + "\n" + code

        # Generate main function with test harness
        main_code = """
int main() {
    // Read JSON input from stdin
    std::string input;
    std::getline(std::cin, input);
    
    // Parse JSON parameters
    auto params = json_parse::parse_object(input);
    
"""

        # Generate parameter extraction based on test case
        param_names = list(parameters.keys())
        param_types = self._infer_cpp_types(parameters)

        for name, cpp_type, value in zip(param_names, param_types, parameters.values()):
            if cpp_type == "int":
                main_code += f'    int {name} = std::stoi(params["{name}"]);\n'
            elif cpp_type == "double":
                main_code += f'    double {name} = std::stod(params["{name}"]);\n'
            elif cpp_type == "bool":
                main_code += f'    bool {name} = (params["{name}"] == "true");\n'
            elif cpp_type == "std::string":
                main_code += f'    std::string {name} = params["{name}"];\n'
            elif cpp_type == "std::vector<int>":
                main_code += f'    std::vector<int> {name} = json_parse::parse_int_array(params["{name}"]);\n'
            elif cpp_type == "std::vector<std::string>":
                main_code += f'    std::vector<std::string> {name} = json_parse::parse_string_array(params["{name}"]);\n'

        # Generate function call based on inplace mode
        if inplace_mode == "0":
            # Normal function call - function returns a value
            if param_names:
                main_code += f"""
    // Call the function
    auto result = {function_name}({', '.join(param_names)});
    
    // Output result as JSON
    std::cout << to_json(result) << std::endl;
"""
            else:
                main_code += f"""
    // Call the function
    auto result = {function_name}();
    
    // Output result as JSON
    std::cout << to_json(result) << std::endl;
"""
        elif inplace_mode == "1":
            # Function modifies arguments in-place
            if param_names:
                first_param = param_names[0]
                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )

                if other_params:
                    main_code += f"""
    // Call the function (modifies first parameter)
    {function_name}({first_param}, {other_params});
    
    // Output modified parameter as JSON
    std::cout << to_json({first_param}) << std::endl;
"""
                else:
                    main_code += f"""
    // Call the function (modifies parameter)
    {function_name}({first_param});
    
    // Output modified parameter as JSON
    std::cout << to_json({first_param}) << std::endl;
"""
            else:
                main_code += f"""
    // Call the function
    {function_name}();
    std::cout << "null" << std::endl;
"""
        elif inplace_mode == "2":
            # Function both modifies in-place and returns a value
            if param_names:
                first_param = param_names[0]
                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )

                if other_params:
                    main_code += f"""
    // Call the function (modifies first parameter and returns value)
    auto result = {function_name}({first_param}, {other_params});
    
    // Output return value
    std::cout << to_json(result) << std::endl;
"""
                else:
                    main_code += f"""
    // Call the function
    auto result = {function_name}({first_param});
    
    // Output return value
    std::cout << to_json(result) << std::endl;
"""
            else:
                main_code += f"""
    // Call the function
    auto result = {function_name}();
    std::cout << to_json(result) << std::endl;
"""

        main_code += """
    return 0;
}
"""

        # Combine everything
        return code + "\n" + main_code

    def _infer_cpp_types(self, parameters: Dict[str, Any]) -> List[str]:
        """Infer C++ types from parameter values."""
        types = []
        for value in parameters.values():
            if isinstance(value, bool):
                types.append("bool")
            elif isinstance(value, int):
                types.append("int")
            elif isinstance(value, float):
                types.append("double")
            elif isinstance(value, str):
                types.append("std::string")
            elif isinstance(value, list):
                if not value:
                    types.append("std::vector<int>")  # Default to int vector
                elif isinstance(value[0], int):
                    types.append("std::vector<int>")
                elif isinstance(value[0], str):
                    types.append("std::vector<std::string>")
                elif isinstance(value[0], float):
                    types.append("std::vector<double>")
                else:
                    types.append("std::vector<int>")  # Default
            else:
                types.append("auto")  # Let compiler deduce
        return types

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile C++ code with appropriate flags."""
        output_path = code_path.replace(self.file_ext, "")
        cmd = self.compile_cmd + ["-o", output_path, code_path, "-std=c++17", "-O2"]

        import subprocess

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return (True, output_path, "")
        else:
            return (False, output_path, result.stderr)
