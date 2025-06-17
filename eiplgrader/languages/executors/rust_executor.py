"""Rust language executor for code testing."""

import os
import json
import tempfile
from typing import Dict, Any, Tuple, List
from .base_executors import CompiledLanguageExecutor


class RustExecutor(CompiledLanguageExecutor):
    """Executor for Rust language code testing."""

    def __init__(self):
        super().__init__(compile_cmd=["rustc"], run_cmd=["./"], file_ext=".rs")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Rust code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # Add necessary use statements
        imports = """use std::io;
use serde_json::{json, Value};

"""

        # Generate main function with test harness
        main_code = """
fn main() {
    // Read test parameters from stdin
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Failed to read input");
    
    let params: Value = serde_json::from_str(&input).expect("Failed to parse JSON");
    
    // Extract parameters
"""

        # Generate parameter extraction based on test case
        param_names = list(parameters.keys())
        param_types = self._infer_rust_types(parameters)

        for name, rust_type in zip(param_names, param_types):
            if rust_type == "i32":
                main_code += (
                    f'    let {name} = params["{name}"].as_i64().unwrap() as i32;\n'
                )
            elif rust_type == "i64":
                main_code += f'    let {name} = params["{name}"].as_i64().unwrap();\n'
            elif rust_type == "f64":
                main_code += f'    let {name} = params["{name}"].as_f64().unwrap();\n'
            elif rust_type == "String":
                main_code += f'    let {name} = params["{name}"].as_str().unwrap().to_string();\n'
            elif rust_type == "&str":
                main_code += f'    let {name}_string = params["{name}"].as_str().unwrap().to_string();\n'
                main_code += f"    let {name} = {name}_string.as_str();\n"
            elif rust_type == "bool":
                main_code += f'    let {name} = params["{name}"].as_bool().unwrap();\n'
            elif rust_type == "Vec<i32>":
                main_code += f"""    let {name}: Vec<i32> = params["{name}"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_i64().unwrap() as i32)
        .collect();
"""
            elif rust_type == "Vec<String>":
                main_code += f"""    let {name}: Vec<String> = params["{name}"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_str().unwrap().to_string())
        .collect();
"""

        # Generate function call based on inplace mode
        if inplace_mode == "0":
            # Normal function call - function returns a value
            main_code += f"""
    // Call the function
    let result = {function_name}({', '.join(param_names)});
    
    // Output result as JSON
    println!("{{}}", serde_json::to_string(&result).unwrap());
"""
        elif inplace_mode == "1":
            # Function modifies arguments in-place
            if param_names:
                first_param = param_names[0]
                # Make the first parameter mutable
                main_code = main_code.replace(
                    f"let {first_param}", f"let mut {first_param}"
                )

                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )
                if other_params:
                    main_code += f"""
    // Call the function (modifies first parameter)
    {function_name}(&mut {first_param}, {other_params});
    
    // Output modified parameter as JSON
    println!("{{}}", serde_json::to_string(&{first_param}).unwrap());
"""
                else:
                    main_code += f"""
    // Call the function (modifies parameter)
    {function_name}(&mut {first_param});
    
    // Output modified parameter as JSON
    println!("{{}}", serde_json::to_string(&{first_param}).unwrap());
"""
            else:
                main_code += f"""
    // Call the function
    {function_name}();
    println!("null");
"""
        elif inplace_mode == "2":
            # Function both modifies in-place and returns a value
            if param_names:
                first_param = param_names[0]
                # Make the first parameter mutable
                main_code = main_code.replace(
                    f"let {first_param}", f"let mut {first_param}"
                )

                other_params = (
                    ", ".join(param_names[1:]) if len(param_names) > 1 else ""
                )
                if other_params:
                    main_code += f"""
    // Call the function (modifies first parameter and returns value)
    match {function_name}(&mut {first_param}, {other_params}) {{
        Ok(result) => println!("{{}}", serde_json::to_string(&result).unwrap()),
        Err(_) => println!("{{}}", serde_json::to_string(&{first_param}).unwrap()),
    }}
"""
                else:
                    main_code += f"""
    // Call the function
    match {function_name}(&mut {first_param}) {{
        Ok(result) => println!("{{}}", serde_json::to_string(&result).unwrap()),
        Err(_) => println!("{{}}", serde_json::to_string(&{first_param}).unwrap()),
    }}
"""
            else:
                main_code += f"""
    // Call the function
    let result = {function_name}();
    println!("{{}}", serde_json::to_string(&result).unwrap());
"""

        main_code += "}\n"

        # Create Cargo.toml content for dependencies
        cargo_toml = """[package]
name = "test"
version = "0.1.0"
edition = "2021"

[dependencies]
serde_json = "1.0"
"""

        # Store Cargo.toml path for later use
        self.cargo_toml_content = cargo_toml

        # Combine everything
        return imports + code + "\n" + main_code

    def _infer_rust_types(self, parameters: Dict[str, Any]) -> List[str]:
        """Infer Rust types from parameter values."""
        types = []
        for value in parameters.values():
            if isinstance(value, bool):
                types.append("bool")
            elif isinstance(value, int):
                # Use i32 for smaller integers, i64 for larger
                if -2147483648 <= value <= 2147483647:
                    types.append("i32")
                else:
                    types.append("i64")
            elif isinstance(value, float):
                types.append("f64")
            elif isinstance(value, str):
                # Default to &str for function parameters
                types.append("&str")
            elif isinstance(value, list):
                if not value:
                    types.append("Vec<i32>")  # Default to Vec<i32>
                elif isinstance(value[0], int):
                    types.append("Vec<i32>")
                elif isinstance(value[0], str):
                    types.append("Vec<String>")
                elif isinstance(value[0], float):
                    types.append("Vec<f64>")
                else:
                    types.append("Vec<Value>")  # Generic JSON value
            else:
                types.append("Value")  # Generic JSON value
        return types

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile Rust code, return (success, output_path, error)"""
        output_path = code_path.replace(self.file_ext, "")

        # Create a temporary Cargo project for better dependency management
        import subprocess

        # Try to compile with rustc directly first
        cmd = ["rustc", "-o", output_path, code_path]
        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return (True, output_path, "")

        # If direct compilation fails, try with explicit serde_json path
        # This is a fallback for when serde_json is not in the default path
        try:
            # Create a minimal Cargo project structure
            project_dir = os.path.join(self.temp_dir, "rust_project")
            os.makedirs(os.path.join(project_dir, "src"), exist_ok=True)

            # Write Cargo.toml
            with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
                f.write(self.cargo_toml_content)

            # Copy the source file
            import shutil

            shutil.copy(code_path, os.path.join(project_dir, "src", "main.rs"))

            # Build with cargo
            build_result = subprocess.run(
                ["cargo", "build", "--release"],
                cwd=project_dir,
                capture_output=True,
                text=True,
            )

            if build_result.returncode == 0:
                # Copy the built executable to the expected location
                built_exe = os.path.join(project_dir, "target", "release", "test")
                if os.path.exists(built_exe):
                    shutil.copy(built_exe, output_path)
                    return (True, output_path, "")

            # If cargo build also fails, return the original error
            return (False, output_path, result.stderr)

        except Exception as e:
            # If cargo approach fails, return the original rustc error
            return (False, output_path, result.stderr)

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Compile and execute test"""
        # Prepare code with test harness
        prepared_code = self.prepare_code(code, test_case)

        # Write to temporary file
        code_path = os.path.join(self.temp_dir, "test.rs")
        with open(code_path, "w") as f:
            f.write(prepared_code)

        # Compile
        success, output_path, error = self.compile(code_path)
        if not success:
            return {
                "passed": False,
                "error": f"Compilation failed: {error}",
                "actual": None,
                "expected": test_case.get("expected"),
            }

        # Execute
        try:
            import subprocess

            # Pass test parameters as stdin
            args_json = json.dumps(test_case.get("parameters", {}))
            result = subprocess.run(
                [output_path],
                input=args_json,
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
            try:
                actual = json.loads(result.stdout.strip())
            except json.JSONDecodeError:
                actual = result.stdout.strip()

            passed = actual == test_case.get("expected")

            return {
                "passed": passed,
                "actual": actual,
                "expected": test_case.get("expected"),
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
