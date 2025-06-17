"""TypeScript language executor for code testing."""

import json
import os
import subprocess
import tempfile
from typing import Dict, Any, Tuple
from ..base import LanguageExecutor


class TypescriptExecutor(LanguageExecutor):
    """Executor for TypeScript language code testing."""

    def __init__(self):
        self.temp_dir = tempfile.mkdtemp()

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare TypeScript code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # Build the test harness
        harness = f"""
// Student code
{code}

// Test harness
(async function runTest(): Promise<void> {{
    try {{
        // Get test parameters from command line or hardcoded
        const testParams = {json.dumps(parameters)};
        const args = Object.values(testParams);
        
        // Check if function exists
        if (typeof {function_name} === 'undefined') {{
            console.error(JSON.stringify({{
                error: "Function '{function_name}' is not defined"
            }}));
            process.exit(1);
        }}
        
        let result: any;
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
        
    }} catch (error: any) {{
        console.error(JSON.stringify({{
            error: error.message || String(error)
        }}));
        process.exit(1);
    }}
}})();
"""
        return harness

    def compile_typescript(self, ts_path: str) -> Tuple[bool, str, str]:
        """Compile TypeScript to JavaScript."""
        js_path = ts_path.replace(".ts", ".js")

        try:
            # Try using tsc first
            result = subprocess.run(
                [
                    "tsc",
                    "--target",
                    "ES2017",
                    "--module",
                    "commonjs",
                    "--lib",
                    "ES2017",
                    "--outDir",
                    self.temp_dir,
                    ts_path,
                ],
                capture_output=True,
                text=True,
            )

            if result.returncode == 0:
                return True, js_path, ""
            else:
                return False, "", result.stdout + result.stderr

        except FileNotFoundError:
            # If tsc is not available, try ts-node directly
            return True, ts_path, ""  # ts-node can execute .ts files directly

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute TypeScript code with test case."""
        try:
            # Prepare code with test harness
            prepared_code = self.prepare_code(code, test_case)

            # Write to temporary TypeScript file
            ts_path = os.path.join(self.temp_dir, "test.ts")
            with open(ts_path, "w") as f:
                f.write(prepared_code)

            # Try to compile TypeScript to JavaScript
            success, js_path, compile_error = self.compile_typescript(ts_path)

            if not success and compile_error:
                return {
                    "passed": False,
                    "error": f"TypeScript compilation failed: {compile_error}",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            # Execute with either node (if compiled) or ts-node (if not)
            try:
                if os.path.exists(js_path) and js_path.endswith(".js"):
                    # Use node for compiled JavaScript
                    result = subprocess.run(
                        ["node", js_path],
                        capture_output=True,
                        text=True,
                        timeout=test_case.get("timeout", 30),
                    )
                else:
                    # Use ts-node for direct TypeScript execution
                    result = subprocess.run(
                        ["ts-node", "--transpile-only", ts_path],
                        capture_output=True,
                        text=True,
                        timeout=test_case.get("timeout", 30),
                    )

                if result.returncode != 0:
                    # Try to parse JSON error from stderr
                    stderr_content = result.stderr.strip()
                    try:
                        error_json = json.loads(stderr_content)
                        if "error" in error_json:
                            error_msg = error_json["error"]
                        else:
                            error_msg = stderr_content
                    except json.JSONDecodeError:
                        error_msg = f"Runtime error: {stderr_content}"

                    return {
                        "passed": False,
                        "error": error_msg,
                        "actual": None,
                        "expected": test_case.get("expected"),
                    }

                # Parse output
                try:
                    actual = json.loads(result.stdout.strip())
                except json.JSONDecodeError:
                    actual = result.stdout.strip()

                passed = actual == test_case.get("expected")

                # Build result
                result_dict = {
                    "passed": passed,
                    "actual": actual,
                    "expected": test_case.get("expected"),
                    "output": result.stdout,
                }

                # Add function call representation if successful
                if "error" not in result_dict:
                    params = test_case.get("parameters", {})
                    function_name = test_case.get("function_name", "foo")
                    args = list(params.values())
                    result_dict["function_call"] = (
                        f"{function_name}({', '.join(map(repr, args))})"
                    )

                return result_dict

            except subprocess.TimeoutExpired:
                return {
                    "passed": False,
                    "error": "Execution timeout",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }
            except FileNotFoundError as e:
                return {
                    "passed": False,
                    "error": "Neither node nor ts-node is installed",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

        except Exception as e:
            return {
                "passed": False,
                "error": str(e),
                "actual": None,
                "expected": test_case.get("expected"),
            }

    def cleanup(self) -> None:
        """Clean up temporary directory."""
        import shutil

        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)
