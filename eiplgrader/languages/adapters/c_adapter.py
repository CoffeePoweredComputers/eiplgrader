"""C language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class CAdapter(LanguageAdapter):
    """Adapter for C language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return C language configuration."""
        return LanguageConfig(
            name="c",
            display_name="C",
            file_extensions=[".c"],
            compile_command=["gcc"],
            run_command=None,  # C is compiled, not interpreted
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate C-specific prompt for LLM."""

        # Base student model for C
        prompt = """Pretend you are an introductory CS student learning C for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic C syntax including headers, pointers,
and memory management patterns.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
The function should follow C conventions.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a C code block:

```c
#include <stdio.h>
#include <stdlib.h>

returnType {function_name}(parameters) {{
    // function body
}}
```

Include necessary headers like stdio.h and stdlib.h if needed.
Use proper C types (int, char, float, double, etc.).
If working with arrays, remember C arrays are passed as pointers.
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function signature: {function_name}({params})
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
C code block. It is very important that you use the provided function name
when generating the code and include necessary headers. For example:

```c
#include <stdio.h>
#include <stdlib.h>

returnType {function_name}({params}) {{
    // function implementation
}}
```

Use standard C conventions for parameter passing:
- Pass by value for basic types
- Pass pointers for arrays or when modifications are needed
- Use const for read-only pointer parameters
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. Each version should be a complete, valid C function.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract C code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```c\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```c
        if "```c" in llm_response:
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split("```c"))
            )[1:]
            return functions

        # Method 3: Look for function definitions
        func_pattern = r"(\w+\s+\w+\s*\([^)]*\)[^{]*{[^}]*})"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL)
        if func_matches:
            # Add headers if missing
            processed_matches = []
            for match in func_matches:
                if "#include" not in match:
                    match = (
                        "#include <stdio.h>\n#include <stdlib.h>\n\n" + match.strip()
                    )
                processed_matches.append(match)
            return processed_matches

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate C syntax using gcc."""
        import subprocess
        import tempfile

        # Ensure code has necessary headers
        if "#include" not in code:
            code = "#include <stdio.h>\n#include <stdlib.h>\n\n" + code

        try:
            # Write code to temporary file
            with tempfile.NamedTemporaryFile(mode="w", suffix=".c", delete=False) as f:
                f.write(code)
                # Add a dummy main function for syntax checking
                f.write("\n\nint main() { return 0; }")
                temp_path = f.name

            # Use gcc to check syntax (-fsyntax-only flag)
            result = subprocess.run(
                ["gcc", "-fsyntax-only", temp_path], capture_output=True, text=True
            )

            # Clean up
            import os

            os.unlink(temp_path)

            if result.returncode == 0:
                return True, None
            else:
                return False, f"Syntax error: {result.stderr}"

        except FileNotFoundError:
            # gcc not available, skip validation
            return True, None
        except Exception as e:
            return False, f"Validation error: {str(e)}"
