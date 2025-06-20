"""Haskell language executor for code testing."""

import os
import json
import subprocess
from typing import Dict, Any, List, Tuple
from .base_executors import CompiledLanguageExecutor


class HaskellExecutor(CompiledLanguageExecutor):
    """Executor for Haskell language code testing."""

    def __init__(self):
        super().__init__(compile_cmd=["ghc"], run_cmd=None, file_ext=".hs")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Haskell code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        expected = test_case.get("expected")
        inplace_mode = test_case.get("inplace", "0")

        # For Haskell, we'll create a complete module with a main function
        prepared_code = "module Main where\n\n"

        # Add necessary imports
        prepared_code += "import System.IO\n"
        prepared_code += "import Data.List (intercalate)\n"
        prepared_code += "import Text.Read (readMaybe)\n\n"

        # Add the student's code
        prepared_code += code + "\n\n"

        # Generate helper functions for JSON-like output
        prepared_code += """
-- Helper functions for output formatting
showList' :: Show a => [a] -> String
showList' xs = "[" ++ intercalate ", " (map show xs) ++ "]"

showString' :: String -> String
showString' s = "\"" ++ s ++ "\""

-- Super simple JSON parsing (no external libraries)
extractValue :: String -> String -> String
extractValue key json = 
    let searchStr = '"' : key ++ "":"
        afterKey = dropWhile (not . (searchStr `isPrefixOf`)) $ words json
    in case afterKey of
        (w:_) -> takeWhile (`notElem` ",}") $ dropWhile (`elem` ": ") $ drop (length searchStr) w
        [] -> error $ "Key not found: " ++ key

getInt :: String -> String -> Int
getInt key json = read $ extractValue key json

getString :: String -> String -> String  
getString key json = 
    let val = extractValue key json
    in if head val == '"' && last val == '"' 
       then init $ tail val  -- Remove quotes
       else val

getDouble :: String -> String -> Double
getDouble key json = read $ extractValue key json

getIntArray :: String -> String -> [Int]
getIntArray key json = 
    let val = extractValue key json
        cleaned = filter (`notElem` "[],") val
    in map read $ words cleaned

getBool :: String -> String -> Bool
getBool key json = 
    let val = extractValue key json
    in case val of
        "true" -> True
        "false" -> False
        _ -> error $ "Invalid bool value: " ++ val

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

"""

        # Generate main function based on test parameters
        prepared_code += "main :: IO ()\n"
        prepared_code += "main = do\n"
        prepared_code += "    hSetBuffering stdout NoBuffering\n"
        prepared_code += "    inputStr <- getLine\n"

        # Parse parameters from JSON input
        param_names = list(parameters.keys())
        param_values = list(parameters.values())

        # Generate parameter parsing code
        for i, (name, value) in enumerate(zip(param_names, param_values)):
            if isinstance(value, int):
                prepared_code += f'    let {name} = getInt "{name}" inputStr\n'
            elif isinstance(value, float):
                prepared_code += f'    let {name} = getDouble "{name}" inputStr\n'
            elif isinstance(value, str):
                prepared_code += f'    let {name} = getString "{name}" inputStr\n'
            elif isinstance(value, bool):
                prepared_code += f'    let {name} = getBool "{name}" inputStr\n'
            elif isinstance(value, list) and all(isinstance(x, int) for x in value):
                prepared_code += f'    let {name} = getIntArray "{name}" inputStr\n'
            elif isinstance(value, list):
                # For now, handle other list types as strings
                prepared_code += f"    let {name} = {value}\n"

        # Generate function call and output based on inplace mode
        if inplace_mode == "0":
            # Normal function call - function returns a value
            if param_names:
                prepared_code += (
                    f"    let result = {function_name} {' '.join(param_names)}\n"
                )
            else:
                prepared_code += f"    let result = {function_name}\n"

            # Output based on expected type
            if isinstance(expected, bool):
                prepared_code += '    putStrLn $ if result then "true" else "false"\n'
            elif isinstance(expected, int):
                prepared_code += "    print result\n"
            elif isinstance(expected, float):
                prepared_code += "    print result\n"
            elif isinstance(expected, str):
                prepared_code += "    putStrLn $ showString' result\n"
            elif isinstance(expected, list):
                prepared_code += "    putStrLn $ showList' result\n"
            else:
                prepared_code += "    print result\n"

        elif inplace_mode == "1":
            # In Haskell, there's no in-place modification due to immutability
            # We'll simulate it by returning the modified value
            if param_names:
                prepared_code += (
                    f"    let result = {function_name} {' '.join(param_names)}\n"
                )
                # Output the "modified" first parameter
                if isinstance(parameters.get(param_names[0]), list):
                    prepared_code += "    putStrLn $ showList' result\n"
                else:
                    prepared_code += "    print result\n"
            else:
                prepared_code += f"    let result = {function_name}\n"
                prepared_code += '    putStrLn "null"\n'

        elif inplace_mode == "2":
            # Function both modifies and returns (simulated in Haskell)
            if param_names:
                prepared_code += (
                    f"    let result = {function_name} {' '.join(param_names)}\n"
                )
                prepared_code += "    print result\n"
            else:
                prepared_code += f"    let result = {function_name}\n"
                prepared_code += "    print result\n"

        return prepared_code

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile Haskell code, return (success, output_path, error)"""
        output_path = code_path.replace(self.file_ext, "")
        # Use -O0 for faster compilation during testing
        cmd = self.compile_cmd + ["-O0", "-o", output_path, code_path]

        result = subprocess.run(cmd, capture_output=True, text=True)

        # Filter out common warnings that don't affect functionality
        if result.returncode == 0:
            return True, output_path, ""
        else:
            # Clean up error messages
            error_lines = result.stderr.strip().split("\n")
            error_msg = "\n".join(
                [
                    line
                    for line in error_lines
                    if line and not line.startswith("[") and "Warning:" not in line
                ]
            )
            return False, output_path, error_msg

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute Haskell test with proper JSON parsing."""
        try:
            # Override parent's execute_test to handle Haskell's output format better
            result = super().execute_test(code, test_case)

            # If the test passed but the output needs cleaning
            if result.get("output") and "actual" in result:
                output = result["output"].strip()

                # Handle Haskell's string output (remove quotes if present)
                if output.startswith('"') and output.endswith('"'):
                    actual = output[1:-1]
                    result["actual"] = actual
                elif output in ("True", "true"):
                    result["actual"] = True
                elif output in ("False", "false"):
                    result["actual"] = False
                else:
                    # Try to parse as JSON first, then as literal
                    try:
                        result["actual"] = json.loads(output)
                    except:
                        try:
                            # Try to evaluate as Python literal (for lists, etc)
                            import ast

                            result["actual"] = ast.literal_eval(output)
                        except:
                            result["actual"] = output

                # Re-check if test passed with cleaned actual value
                result["passed"] = result["actual"] == test_case.get("expected")

            return result

        except Exception as e:
            return {
                "passed": False,
                "error": str(e),
                "actual": None,
                "expected": test_case.get("expected"),
            }
