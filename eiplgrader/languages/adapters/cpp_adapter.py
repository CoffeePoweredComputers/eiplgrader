"""C++ language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class CppAdapter(LanguageAdapter):
    """Adapter for C++ language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return C++ language configuration."""
        return LanguageConfig(
            name="cpp",
            display_name="C++",
            file_extensions=[".cpp", ".cc", ".cxx"],
            compile_command=["g++", "-std=c++17"],
            run_command=["./a.out"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate C++-specific prompt for LLM."""

        # Base student model for C++
        prompt = """Pretend you are an introductory CS student learning C++ for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic C++ syntax including headers, namespaces,
and the Standard Template Library (STL). You know about references and pointers
but prefer using modern C++ features when appropriate.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
The function should follow modern C++ conventions (C++17).
If the function works with collections, prefer using STL containers like std::vector,
std::string, std::array over raw arrays and C-style strings.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a C++ code block:

```cpp
#include <iostream>
#include <vector>
// include other necessary headers

{kwargs.get('return_type', 'auto')} {function_name}(parameters) {{
    // function body
}}
```

Use appropriate parameter passing (by value, reference, or const reference) based
on the function's requirements.
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function signature: {kwargs.get('return_type', 'auto')} {function_name}({params})
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
C++ code block. It is very important that you use the provided function name
when generating the code and include necessary headers. For example:

```cpp
#include <iostream>
// include other necessary headers

{kwargs.get('return_type', 'auto')} {function_name}({params}) {{
    // function implementation
}}
```

Use modern C++ features and STL containers where appropriate.
Prefer references over pointers for non-nullable parameters.
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. Each version should be a complete, valid C++ function with all
necessary headers included.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract C++ code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```cpp\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Also check for c++ (lowercase) blocks
        pattern2 = r"```c\+\+\n(.*?)```"
        matches2 = re.findall(pattern2, llm_response, re.DOTALL)
        if matches2:
            return [match.strip() for match in matches2]

        # Method 2: If no markdown blocks, split by ```cpp
        if "```cpp" in llm_response or "```c++" in llm_response:
            delimiter = "```cpp" if "```cpp" in llm_response else "```c++"
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split(delimiter))
            )[1:]
            return functions

        # Method 3: Look for function definitions
        # C++ functions can have various return types and modifiers
        func_pattern = r"((?:(?:inline|static|extern|virtual|const|constexpr)\s+)*(?:\w+(?:::\w+)*(?:<[^>]+>)?(?:\s*[*&]+)?)\s+\w+\s*\([^)]*\)[^{]*{[^}]*})"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL)
        if func_matches:
            # Add common headers if missing
            processed_matches = []
            for match in func_matches:
                if "#include" not in match:
                    headers = (
                        "#include <iostream>\n#include <vector>\n#include <string>\n\n"
                    )
                    match = headers + match.strip()
                processed_matches.append(match)
            return processed_matches

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate C++ syntax using g++ -fsyntax-only."""
        import subprocess
        import tempfile

        # Ensure code has necessary headers for basic compilation
        if "#include" not in code:
            code = "#include <iostream>\n" + code

        try:
            # Write code to temporary file
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".cpp", delete=False
            ) as f:
                f.write(code)
                temp_path = f.name

            # Use g++ to check syntax only
            result = subprocess.run(
                ["g++", "-std=c++17", "-fsyntax-only", temp_path],
                capture_output=True,
                text=True,
            )

            # Clean up
            import os

            os.unlink(temp_path)

            if result.returncode == 0:
                return True, None
            else:
                # Extract meaningful error message
                error_lines = result.stderr.strip().split("\n")
                error_msg = "\n".join(
                    line
                    for line in error_lines
                    if "error:" in line or "warning:" in line
                )[
                    :200
                ]  # Limit error message length
                return False, f"Syntax error: {error_msg}"

        except FileNotFoundError:
            # g++ not available, skip validation
            return True, None
        except Exception as e:
            return False, f"Validation error: {str(e)}"
