"""Python language adapter."""

import re
import ast
from typing import List
from ..base import LanguageAdapter, LanguageConfig


class PythonAdapter(LanguageAdapter):
    """Python language adapter with 4 core methods."""

    def get_config(self) -> LanguageConfig:
        """Return Python language configuration."""
        return LanguageConfig(
            name="python",
            display_name="Python",
            file_extensions=[".py"],
            run_command=["python3"],
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
        """Generate Python-specific prompt for LLM."""
        if gen_type == "cgbg":
            return f"""Pretend you are an introductory CS student learning Python for the very first time. You also don't know about type annotations.

Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a python code block:

```python
def {function_name}():
    pass
```"""

        elif gen_type == "redef":
            function_signature = kwargs.get(
                "function_signature", f"def {function_name}():"
            )
            assumptions = kwargs.get("assumptions", "")

            return f"""Pretend you are an introductory CS student learning Python for the very first time. You also don't know about type annotations.

Create a function based on the following function signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
python code block. It is very important that you use the provided function name
when generating the code. For example:

```python
{function_signature}
    pass
```"""

        else:
            return f"Generate a Python function named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Python code blocks from LLM response."""
        patterns = [
            r"```python\n(.*?)\n```",
            r"```py\n(.*?)\n```",
            r"```\n(.*?)\n```",
        ]

        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize Python code by removing comments and standardizing format."""
        lines = []
        for line in code.split("\n"):
            # Remove comments (everything after #)
            if "#" in line:
                line = line[: line.index("#")]

            # Skip empty lines and whitespace-only lines
            stripped = line.strip()
            if stripped:
                lines.append(stripped)

        # Join lines and normalize whitespace
        if not lines:
            return ""

        normalized = " ".join(lines)
        normalized = re.sub(r"\s+", " ", normalized)
        return normalized.strip()
