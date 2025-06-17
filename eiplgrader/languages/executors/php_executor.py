"""PHP language executor for code testing."""

import json
import textwrap
from typing import Dict, Any
from ..executors.base_executors import InterpretedLanguageExecutor


class PhpExecutor(InterpretedLanguageExecutor):
    """Executor for PHP language code testing."""

    def __init__(self):
        super().__init__(interpreter_cmd=["php"], file_ext=".php")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare PHP code for execution with test harness."""
        # Ensure code has PHP tag
        if not code.strip().startswith("<?php"):
            code = "<?php\n" + code

        # Get function name and parameters
        function_name = test_case.get("function_name", "foo")
        params = test_case.get("parameters", {})
        expected = test_case.get("expected")
        inplace_mode = test_case.get("inplace", "0")

        # Build the test harness
        test_harness = f"""
{code}

// Test harness
$test_params = json_decode('{json.dumps(params)}', true);
$expected = json_decode('{json.dumps(expected)}', true);

// Extract parameters in order
$args = array_values($test_params);

// Handle different inplace modes
$inplace_mode = '{inplace_mode}';

try {{
    if ($inplace_mode === '0') {{
        // Normal function call - function returns a value
        $actual = call_user_func_array('{function_name}', $args);
    }} elseif ($inplace_mode === '1') {{
        // Function modifies arguments in-place
        if (count($args) > 0) {{
            // Make a copy of the first argument
            $actual = $args[0];
            if (is_array($actual)) {{
                $actual = json_decode(json_encode($actual), true); // Deep copy
            }}
            // Call function with the copied first argument
            $call_args = array(&$actual);
            for ($i = 1; $i < count($args); $i++) {{
                $call_args[] = $args[$i];
            }}
            call_user_func_array('{function_name}', $call_args);
        }} else {{
            $actual = {function_name}();
        }}
    }} elseif ($inplace_mode === '2') {{
        // Function both modifies in-place and returns a value
        if (count($args) > 0) {{
            // Make a copy of the first argument
            $modified_arg = $args[0];
            if (is_array($modified_arg)) {{
                $modified_arg = json_decode(json_encode($modified_arg), true); // Deep copy
            }}
            // Call function with the copied first argument
            $call_args = array(&$modified_arg);
            for ($i = 1; $i < count($args); $i++) {{
                $call_args[] = $args[$i];
            }}
            $result = call_user_func_array('{function_name}', $call_args);
            // Use return value if available, otherwise use modified argument
            $actual = ($result !== null) ? $result : $modified_arg;
        }} else {{
            $actual = {function_name}();
        }}
    }} else {{
        throw new Exception("Invalid inplace mode: $inplace_mode");
    }}
    
    // Output result as JSON
    echo json_encode($actual);
    
}} catch (Exception $e) {{
    // Output error
    echo json_encode(array("__error__" => $e->getMessage()));
}}
"""

        return test_harness

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute PHP code with test case."""
        result = super().execute_test(code, test_case)

        # Check if result contains error marker
        if isinstance(result.get("actual"), dict) and "__error__" in result.get(
            "actual", {}
        ):
            error_msg = result["actual"]["__error__"]
            result["passed"] = False
            result["error"] = error_msg
            result["actual"] = None

        # Add function call string for display
        if "error" not in result:
            function_name = test_case.get("function_name", "foo")
            params = test_case.get("parameters", {})
            args = list(params.values())

            # Format PHP-style function call
            arg_strs = []
            for arg in args:
                if isinstance(arg, str):
                    arg_strs.append(f'"{arg}"')
                elif isinstance(arg, (list, dict)):
                    arg_strs.append(f"array({json.dumps(arg)})")
                else:
                    arg_strs.append(str(arg))

            result["function_call"] = f"{function_name}({', '.join(arg_strs)})"

        return result
