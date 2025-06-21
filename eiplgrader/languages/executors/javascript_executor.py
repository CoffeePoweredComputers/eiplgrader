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

        # Build the test harness
        harness = f"""
// Student code
{code}

// Test harness
(async function runTest() {{
    try {{
        // Get test parameters from command line or hardcoded
        const testParams = {json.dumps(parameters)};
        const paramTypes = {json.dumps(parameter_types)};
        const expectedType = {json.dumps(expected_type)};
        
        // Validate parameter types match expected format
        const paramNames = Object.keys(testParams);
        const args = [];
        
        for (const paramName of paramNames) {{
            if (!paramTypes[paramName]) {{
                console.error(JSON.stringify({{
                    error: `Missing type for parameter: ${{paramName}}`
                }}));
                process.exit(1);
            }}
            args.push(testParams[paramName]);
        }}
        
        // Check if function exists
        if (typeof {function_name} === 'undefined') {{
            console.error(JSON.stringify({{
                error: "Function '{function_name}' is not defined"
            }}));
            process.exit(1);
        }}
        
        let result;
        const inplaceMode = "{inplace_mode}";
        
        if (inplaceMode === "0") {{
            // Normal function call - function returns a value
            result = await {function_name}(...args);
        }} else if (inplaceMode === "1") {{
            // Function modifies arguments in-place
            if (args.length > 0) {{
                // Deep copy first argument
                let firstArg = JSON.parse(JSON.stringify(args[0]));
                await {function_name}(firstArg, ...args.slice(1));
                result = firstArg;
            }} else {{
                result = await {function_name}();
            }}
        }} else if (inplaceMode === "2") {{
            // Function both modifies in-place and returns a value
            if (args.length > 0) {{
                // Deep copy first argument
                let firstArg = JSON.parse(JSON.stringify(args[0]));
                let returnValue = await {function_name}(firstArg, ...args.slice(1));
                // Use return value if it exists, otherwise use modified argument
                result = returnValue !== undefined ? returnValue : firstArg;
            }} else {{
                result = await {function_name}();
            }}
        }} else {{
            console.error(JSON.stringify({{
                error: `Invalid inplace mode: ${{inplaceMode}}`
            }}));
            process.exit(1);
        }}
        
        // Output result as JSON
        console.log(JSON.stringify(result));
        
    }} catch (error) {{
        console.error(JSON.stringify({{
            error: error.message || String(error)
        }}));
        process.exit(1);
    }}
}})();
"""
        return harness

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
