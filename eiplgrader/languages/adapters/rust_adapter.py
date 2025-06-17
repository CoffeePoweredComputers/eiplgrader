"""Rust language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class RustAdapter(LanguageAdapter):
    """Adapter for Rust language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return Rust language configuration."""
        return LanguageConfig(
            name="rust",
            display_name="Rust",
            file_extensions=[".rs"],
            compile_command=["rustc"],
            run_command=["cargo", "run"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Rust-specific prompt for LLM."""

        # Base student model for Rust
        prompt = """Pretend you are an introductory CS student learning Rust for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Rust syntax including ownership, borrowing,
lifetimes, and error handling with Result types.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
The function should follow Rust conventions and handle errors using Result<T, E> when appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a Rust code block:

```rust
fn {function_name}(parameters) -> ReturnType {{
    // function body
}}
```

If the function can fail, use Rust's idiomatic error handling pattern with Result<T, E>.
Follow Rust ownership rules and borrowing conventions. Use references (&) when you don't
need to take ownership of parameters.
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function signature: fn {function_name}({params}) -> ReturnType
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
Rust code block. It is very important that you use the provided function name
when generating the code. For example:

```rust
fn {function_name}({params}) -> ReturnType {{
    // function implementation
}}
```

If the function should handle errors, use Result<T, E> as the return type
following Rust conventions. Follow ownership and borrowing rules - use references
when appropriate and avoid unnecessary clones.
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. Each version should be a complete, valid Rust function that follows
ownership and borrowing rules.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Rust code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```rust\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```rust
        if "```rust" in llm_response:
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split("```rust"))
            )[1:]
            return functions

        # Method 3: Look for function definitions
        func_pattern = r"(fn\s+\w+\s*(?:<[^>]*>)?\s*\([^)]*\)[^{]*{.*?})"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL)
        if func_matches:
            return func_matches

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Rust syntax using rustc."""
        import subprocess
        import tempfile

        try:
            # Write code to temporary file
            with tempfile.NamedTemporaryFile(mode="w", suffix=".rs", delete=False) as f:
                # Wrap in a main function if it's just a function definition
                if code.strip().startswith("fn") and "fn main" not in code:
                    f.write(f"{code}\n\nfn main() {{}}")
                else:
                    f.write(code)
                temp_path = f.name

            # Use rustc to check syntax
            result = subprocess.run(
                [
                    "rustc",
                    "--crate-type",
                    "lib",
                    "--emit",
                    "metadata",
                    "-Z",
                    "no-codegen",
                    temp_path,
                ],
                capture_output=True,
                text=True,
                env={
                    **subprocess.os.environ,
                    "RUSTC_BOOTSTRAP": "1",
                },  # Enable unstable features
            )

            # Clean up
            import os

            os.unlink(temp_path)

            if result.returncode == 0:
                return True, None
            else:
                # Extract relevant error message
                error_lines = result.stderr.strip().split("\n")
                relevant_errors = [
                    line for line in error_lines if "error" in line.lower()
                ]
                error_msg = (
                    "\n".join(relevant_errors) if relevant_errors else result.stderr
                )
                return False, f"Syntax error: {error_msg}"

        except FileNotFoundError:
            # rustc not available, skip validation
            return True, None
        except Exception as e:
            return False, f"Validation error: {str(e)}"
