"""Python language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig
import ast


DEFAULT_STUDENT_PERSONA_PYTHON = """
You are an introductory CS student learning Python for the very first time. You
also don't know about type annotations. Do not use list comprehension, lambda's
or maps --- use iteration instead. Additionally, do not consolidate multiple
complex operations into a single line. Break things into multiple distinct
lines wherever possible.
"""

DEFAULT_CGBG_PROMPT_PYTHON = """
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}
"""

DEFAULT_REDEF_PROMPT_PYTHON = """
Create a function based on the following function signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.
"""

DEFAULT_RETURN_FORMAT_PYTHON = """
Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a python code block:

```python
def {function_name}():
    pass
```"""
# Whitespace normalization pattern
EXTRA_BLANK_LINES = r"\n\s*\n"


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

        prompt = DEFAULT_STUDENT_PERSONA_PYTHON

        if gen_type == "cgbg":

            prompt += DEFAULT_CGBG_PROMPT_PYTHON.format(
                function_name=function_name, student_response=student_response
            )

            prompt += DEFAULT_RETURN_FORMAT_PYTHON.format(function_name=function_name)

            return prompt

        elif gen_type == "redef":

            params = kwargs.get("params", "")
            function_signature = f"def {function_name}({params}):"

            prompt += DEFAULT_REDEF_PROMPT_PYTHON.format(
                function_signature=function_signature,
                assumptions=kwargs.get("assumptions", ""),
            )

            prompt += DEFAULT_RETURN_FORMAT_PYTHON.format(function_name=function_name)
            return prompt

        else:
            raise ValueError(
                f"Unsupported generation type: {gen_type}. Supported types are 'cgbg' and 'redef'."
            )

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
        parsed_python = ast.parse(code)
        for node in ast.walk(parsed_python):
            if isinstance(node, ast.Expr) and isinstance(node.value, ast.Str):
                # Remove string literals that are comments
                if re.match(r"^\s*#", node.value.s):
                    parsed_python.body.remove(node)
        code_without_comments = ast.unparse(parsed_python)
        return code_without_comments
