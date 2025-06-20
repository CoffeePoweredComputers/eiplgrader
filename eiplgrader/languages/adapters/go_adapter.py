"""Go language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig


class GoAdapter(LanguageAdapter):
    """Go language adapter with 4 core methods."""

    def get_config(self) -> LanguageConfig:
        """Return Go language configuration."""
        return LanguageConfig(
            name="go",
            display_name="Go",
            file_extensions=[".go"],
            run_command=["go", "run"],
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
        """Generate Go-specific prompt for LLM."""
        if gen_type == "cgbg":
            return f"""Pretend you are an introductory CS student learning Go for the very first time. You know basic Go syntax, goroutines, and channels. Do not use overly complex features unless absolutely necessary. Additionally, do not do multiple complex operations on a single line. Break operations into multiple lines wherever possible.

Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a go code block:

```go
func {function_name}() int {{
    return 0
}}
```"""

        elif gen_type == "redef":
            function_signature = kwargs.get(
                "function_signature", f"func {function_name}()"
            )
            assumptions = kwargs.get("assumptions", "")

            return f"""Pretend you are an introductory CS student learning Go for the very first time. You know basic Go syntax, goroutines, and channels.

Create a function based on the following function signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
go code block. It is very important that you use the provided function name
when generating the code. For example:

```go
{function_signature} {{
    return 0
}}
```"""

        else:
            return f"Generate a Go function named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Go code blocks from LLM response."""
        patterns = [
            r"```go\n(.*?)\n```",
            r"```golang\n(.*?)\n```",
            r"```\n(.*?)\n```",
        ]

        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize Go code by removing comments and standardizing format."""
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
