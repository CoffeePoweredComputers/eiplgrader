"""C++ language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig


class CppAdapter(LanguageAdapter):
    """C++ language adapter with 4 core methods."""

    def get_config(self) -> LanguageConfig:
        """Return C++ language configuration."""
        return LanguageConfig(
            name="cpp",
            display_name="C++",
            file_extensions=[".cpp", ".cc", ".cxx"],
            run_command=["./a.out"],
            compile_command=["g++", "-o", "a.out"],
            test_timeout=30,
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        num_to_gen: int = 1,
        **kwargs,
    ) -> str:
        """Generate C++-specific prompt for LLM."""
        if gen_type == "cgbg":
            return f"""Pretend you are an introductory CS student learning C++ for the very first time. You know basic C++ syntax, object-oriented programming, and STL containers.

Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a cpp code block:

```cpp
int {function_name}() {{
    return 0;
}}
```"""

        elif gen_type == "redef":
            function_signature = kwargs.get(
                "function_signature", f"int {function_name}()"
            )
            assumptions = kwargs.get("assumptions", "")

            return f"""Pretend you are an introductory CS student learning C++ for the very first time. You know basic C++ syntax, object-oriented programming, and STL containers.

Create a function based on the following function signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
cpp code block. It is very important that you use the provided function name
when generating the code. For example:

```cpp
{function_signature} {{
    return 0;
}}
```"""

        else:
            return f"Generate a C++ function named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract C++ code blocks from LLM response."""
        patterns = [
            r"```cpp\n(.*?)\n```",
            r"```c\+\+\n(.*?)\n```",
            r"```\n(.*?)\n```",
        ]

        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize C++ code by removing comments and standardizing format."""
        # Remove single-line comments
        code = re.sub(r"//.*", "", code)

        # Remove multi-line comments
        code = re.sub(r"/\*.*?\*/", "", code, flags=re.DOTALL)

        # Remove empty lines and normalize whitespace
        lines = []
        for line in code.split("\n"):
            stripped = line.strip()
            if stripped:
                lines.append(stripped)

        # Join lines and normalize whitespace
        if not lines:
            return ""

        normalized = " ".join(lines)
        normalized = re.sub(r"\s+", " ", normalized)
        return normalized.strip()
