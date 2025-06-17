"""Go language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class GoAdapter(LanguageAdapter):
    """Adapter for Go language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return Go language configuration."""
        return LanguageConfig(
            name="go",
            display_name="Go",
            file_extensions=[".go"],
            compile_command=["go", "build"],
            run_command=["go", "run"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Go-specific prompt for LLM."""

        # Base student model for Go
        prompt = """Pretend you are an introductory CS student learning Go for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Go syntax including package declarations,
imports, and error handling patterns.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
The function should follow Go conventions and be part of the main package.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a Go code block:

```go
package main

func {function_name}(parameters) returnType {{
    // function body
}}
```

If the function needs to handle errors, use Go's idiomatic error handling pattern
with multiple return values.
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function signature: func {function_name}({params}) returnType
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
Go code block. It is very important that you use the provided function name
when generating the code and include the package declaration. For example:

```go
package main

func {function_name}({params}) returnType {{
    // function implementation
}}
```

If the function should handle errors, include error as the last return value
following Go conventions.
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. Each version should be a complete, valid Go function.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Go code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```go\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```go
        if "```go" in llm_response:
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split("```go"))
            )[1:]
            return functions

        # Method 3: Look for function definitions
        func_pattern = r"(func\s+\w+\s*\([^)]*\)[^{]*{.*?})"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL)
        if func_matches:
            # Add package declaration if missing
            processed_matches = []
            for match in func_matches:
                if "package main" not in match:
                    match = "package main\n\n" + match.strip()
                processed_matches.append(match)
            return processed_matches

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Go syntax using gofmt."""
        import subprocess
        import tempfile

        # Ensure code has package declaration
        if not code.strip().startswith("package"):
            code = "package main\n\n" + code

        try:
            # Write code to temporary file
            with tempfile.NamedTemporaryFile(mode="w", suffix=".go", delete=False) as f:
                f.write(code)
                temp_path = f.name

            # Use gofmt to check syntax
            result = subprocess.run(
                ["gofmt", "-e", temp_path], capture_output=True, text=True
            )

            # Clean up
            import os

            os.unlink(temp_path)

            if result.returncode == 0:
                return True, None
            else:
                return False, f"Syntax error: {result.stderr}"

        except FileNotFoundError:
            # gofmt not available, skip validation
            return True, None
        except Exception as e:
            return False, f"Validation error: {str(e)}"
