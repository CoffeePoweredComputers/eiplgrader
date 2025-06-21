"""JavaScript language executor for code testing."""

import json
import os
from typing import Dict, Any
from ..executors.base_executors import InterpretedLanguageExecutor


class JavaScriptExecutor(InterpretedLanguageExecutor):
    """Executor for JavaScript language code testing."""

    def __init__(self):
        super().__init__(interpreter_cmd=["node"], file_ext=".js")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare JavaScript code for execution with test harness."""
        # For JavaScript, types are optional - infer if not provided
        if "parameter_types" not in test_case:
            test_case["parameter_types"] = {}
            for param_name, value in test_case.get("parameters", {}).items():
                test_case["parameter_types"][param_name] = self.infer_type(value)
    
        if "expected_type" not in test_case:
            test_case["expected_type"] = self.infer_type(test_case.get("expected"))
    
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        parameter_types = test_case["parameter_types"]  # Required field
        expected_type = test_case["expected_type"]  # Required field
        inplace_mode = test_case.get("inplace", "0")
    
        # Import template functions
        from .templates import (
            generate_javascript_parameter_setup,
            generate_javascript_function_check,
            generate_inplace_function_call,
        )
        from .string_utils import CodeBuilder
    
        builder = CodeBuilder()
    
        # Add student code
        builder.add_line("// Student code")
        builder.add_lines(code)
        builder.add_line()
    
        # Build the inner test harness content
        inner_builder = CodeBuilder()
        
        # Add parameter setup
        param_setup = generate_javascript_parameter_setup(
            parameters, parameter_types, expected_type
        )
        inner_builder.add_lines(param_setup)
        inner_builder.add_line()
    
        # Add function existence check
        function_check = generate_javascript_function_check(function_name)
        inner_builder.add_lines(function_check)
        inner_builder.add_line()
    
        # Add result handling based on inplace mode
        inner_builder.add_line("let result;")
        inner_builder.add_line(f'const inplaceMode = "{inplace_mode}";')
        inner_builder.add_line()
    
        # Generate inplace function call handling
        param_names = list(parameters.keys()) if parameters else []
        inplace_call_code = generate_inplace_function_call(
            "javascript", function_name, param_names, inplace_mode, parameter_types, expected_type
        )
        
        # Parse the inplace call code to extract just the logic we need
        # The generate_inplace_function_call returns the result handling
        if inplace_mode == "0":
            inner_builder.add_line('if (inplaceMode === "0") {')
            with inner_builder.indent():
                inner_builder.add_line("// Normal function call - function returns a value")
                if param_names:
                    inner_builder.add_line(f"result = await {function_name}(...args);")
                else:
                    inner_builder.add_line(f"result = await {function_name}();")
            inner_builder.add_line("}")
        elif inplace_mode == "1":
            inner_builder.add_line('if (inplaceMode === "1") {')
            with inner_builder.indent():
                inner_builder.add_line("// Function modifies arguments in-place")
                inner_builder.add_line("if (args.length > 0) {")
                with inner_builder.indent():
                    inner_builder.add_line("// Deep copy first argument")
                    inner_builder.add_line("let firstArg = JSON.parse(JSON.stringify(args[0]));")
                    inner_builder.add_line(f"await {function_name}(firstArg, ...args.slice(1));")
                    inner_builder.add_line("result = firstArg;")
                inner_builder.add_line("} else {")
                with inner_builder.indent():
                    inner_builder.add_line(f"result = await {function_name}();")
                inner_builder.add_line("}")
            inner_builder.add_line("}")
        elif inplace_mode == "2":
            inner_builder.add_line('if (inplaceMode === "2") {')
            with inner_builder.indent():
                inner_builder.add_line("// Function both modifies in-place and returns a value")
                inner_builder.add_line("if (args.length > 0) {")
                with inner_builder.indent():
                    inner_builder.add_line("// Deep copy first argument")
                    inner_builder.add_line("let firstArg = JSON.parse(JSON.stringify(args[0]));")
                    inner_builder.add_line(f"let returnValue = await {function_name}(firstArg, ...args.slice(1));")
                    inner_builder.add_line("// Use return value if it exists, otherwise use modified argument")
                    inner_builder.add_line("result = returnValue !== undefined ? returnValue : firstArg;")
                inner_builder.add_line("} else {")
                with inner_builder.indent():
                    inner_builder.add_line(f"result = await {function_name}();")
                inner_builder.add_line("}")
            inner_builder.add_line("}")
        
        # Handle all inplace modes with error handling
        if inplace_mode not in ["0", "1", "2"]:
            inner_builder.add_line("if (inplaceMode !== \"0\" && inplaceMode !== \"1\" && inplaceMode !== \"2\") {")
            with inner_builder.indent():
                inner_builder.add_line("console.error(JSON.stringify({")
                with inner_builder.indent():
                    inner_builder.add_line("error: `Invalid inplace mode: ${inplaceMode}`")
                inner_builder.add_line("}));")
                inner_builder.add_line("process.exit(1);")
            inner_builder.add_line("}")
    
        inner_builder.add_line()
    
        # Add output
        inner_builder.add_line("// Output result as JSON")
        inner_builder.add_line("console.log(JSON.stringify(result));")
    
        # Wrap in async function with error handling
        builder.add_line("// Test harness")
        builder.add_line("(async function runTest() {")
        with builder.indent():
            builder.add_line("try {")
            with builder.indent():
                builder.add_lines(inner_builder.build())
            builder.add_line("} catch (error) {")
            with builder.indent():
                builder.add_line("console.error(JSON.stringify({")
                with builder.indent():
                    builder.add_line("error: error.message || String(error)")
                builder.add_line("}));")
                builder.add_line("process.exit(1);")
            builder.add_line("}")
        builder.add_line("})();")
    
        return builder.build()



    # execute_test() method removed - using base class implementation
    # Error handling moved to enhance_error_message() method

    def enhance_error_message(self, error_msg: str, stderr: str = "") -> str:
        """Handle JavaScript-specific error parsing."""
        # Check if "Runtime error:" is in error_msg
        if "Runtime error:" in error_msg:
            # Extract stderr content by removing "Runtime error:" prefix and strip whitespace
            stderr_content = error_msg.replace("Runtime error:", "").strip()
            
            # Try to parse as JSON and extract the "error" field if present
            try:
                error_json = json.loads(stderr_content)
                if "error" in error_json:
                    return error_json["error"]
            except json.JSONDecodeError:
                pass
        
        # Return the original error_msg if no enhancement was possible
        return error_msg
