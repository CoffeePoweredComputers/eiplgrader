"""Bash language executor for code testing."""

import subprocess
import tempfile
import json
import os
import shutil
from typing import Dict, Any
from ..base import LanguageExecutor


class BashExecutor(LanguageExecutor):
    """Executor for Bash language code testing."""

    def __init__(self):
        self.temp_dir = tempfile.mkdtemp()

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Bash code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # Create test harness
        test_harness = f"""#!/bin/bash

# Generated function
{code}

# Test execution
"""

        # Prepare arguments
        args = list(parameters.values())

        if inplace_mode == "0":
            # Normal function call - function returns a value via echo
            # First check if the function exists
            test_harness += f"""
# Check if function exists
if ! type {function_name} &>/dev/null; then
    echo "Error: Function '{function_name}' not found" >&2
    exit 1
fi

# Call the function with arguments
"""
            arg_str = " ".join(
                f'"{arg}"' if isinstance(arg, str) else str(arg) for arg in args
            )
            test_harness += f"""result=$({function_name} {arg_str} 2>&1)
exit_code=$?

# Check for errors
if [ $exit_code -ne 0 ]; then
    echo "Error: Function returned non-zero exit code" >&2
    exit 1
fi

# Output result for parsing
echo "$result"
"""
        elif inplace_mode == "1":
            # In-place modification mode
            # For bash, we'll simulate this by passing array names or using global variables
            # This is a simplified approach - real implementation would need more sophisticated handling
            test_harness += f"""
# For in-place modifications in bash, we typically use nameref or arrays
# This is a simplified implementation
echo "Error: In-place modification mode not fully supported for bash"
exit 1
"""
        elif inplace_mode == "2":
            # Both in-place and return value
            test_harness += f"""
# For combined in-place and return in bash
echo "Error: Combined in-place/return mode not fully supported for bash"
exit 1
"""
        else:
            test_harness += f"""
echo "Error: Invalid inplace mode: {inplace_mode}"
exit 1
"""

        return test_harness

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute Bash code with test case."""
        try:
            # Prepare code with test harness
            prepared_code = self.prepare_code(code, test_case)

            # Write to temporary file
            script_path = os.path.join(self.temp_dir, "test.sh")
            with open(script_path, "w") as f:
                f.write(prepared_code)

            # Make executable
            os.chmod(script_path, 0o755)

            # Execute
            result = subprocess.run(
                ["bash", script_path],
                capture_output=True,
                text=True,
                timeout=test_case.get("timeout", 30),
            )

            if result.returncode != 0:
                return {
                    "passed": False,
                    "error": f"Runtime error: {result.stderr}",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            # Parse output
            actual = result.stdout.strip()

            # Try to parse as JSON if the expected value is a complex type
            expected = test_case.get("expected")
            if isinstance(expected, (list, dict)):
                try:
                    actual = json.loads(actual)
                except json.JSONDecodeError:
                    pass
            elif isinstance(expected, (int, float)):
                # Try to convert to number
                try:
                    if "." in actual:
                        actual = float(actual)
                    else:
                        actual = int(actual)
                except ValueError:
                    pass
            elif isinstance(expected, bool):
                # Convert string representation to boolean
                if actual.lower() in ("true", "1", "yes"):
                    actual = True
                elif actual.lower() in ("false", "0", "no"):
                    actual = False

            # Compare results
            passed = actual == expected

            # Create function call string for display
            function_name = test_case.get("function_name", "foo")
            params = test_case.get("parameters", {})
            args = list(params.values())
            arg_str = " ".join(repr(arg) for arg in args)
            function_call = f"{function_name} {arg_str}"

            return {
                "passed": passed,
                "actual": actual,
                "expected": expected,
                "function_call": function_call,
                "output": result.stdout,
            }

        except subprocess.TimeoutExpired:
            return {
                "passed": False,
                "error": "Execution timeout",
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
        try:
            if os.path.exists(self.temp_dir):
                shutil.rmtree(self.temp_dir)
        except:
            pass
