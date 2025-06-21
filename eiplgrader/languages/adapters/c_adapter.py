"""C language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig


DEFAULT_STUDENT_PERSONA_C = """
Pretend you are an introductory CS student learning C for the very first time. 
You know basic C syntax, pointers, and memory management. 
Do not do things in a single line. 
Break operations into multiple lines wherever possible.
"""

DEFAULT_CGBG_PROMPT_C = """
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}
"""

DEFAULT_REDEF_PROMPT_C = """
Create a function based on the following function signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.
"""

DEFAULT_RETURN_FORMAT_C = """
Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a c code block:

```c
int {function_name}() {{
    return 0;
}}
```"""

DEFAULT_REDEF_RETURN_FORMAT_C = """
Generate the code only and generate it to be surrounded with markdown of a
c code block. It is very important that you use the provided function name
when generating the code. For example:

```c
{function_signature} {{
    return 0;
}}
```"""

# Regex patterns for code extraction
CODE_BLOCK_PATTERNS = [
    r"```c\n(.*?)\n```",
    r"```\n(.*?)\n```",
]

# Comment patterns
SINGLE_LINE_COMMENT_PATTERN = r"//.*"
SINGLE_INLINE_COMMENT_PATTERN = r"\s*//.*"
MULTI_LINE_COMMENT_PATTERN = r"/\*.*?\*/"

# Whitespace normalization pattern
EXTRA_BLANK_LINES = r"\n\s*\n"


class CAdapter(LanguageAdapter):
    """C language adapter with 4 core methods."""

    def get_config(self) -> LanguageConfig:
        """Return C language configuration."""
        return LanguageConfig(
            name="c",
            display_name="C",
            file_extensions=[".c"],
            run_command=["./a.out"],
            compile_command=["gcc", "-o", "a.out"],
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
        """Generate C-specific prompt for LLM."""

        prompt = DEFAULT_STUDENT_PERSONA_C.strip()

        if gen_type == "cgbg":
            prompt += "\n" + DEFAULT_CGBG_PROMPT_C.format(
                function_name=function_name, student_response=student_response
            )

            prompt += "\n" + DEFAULT_RETURN_FORMAT_C.format(function_name=function_name)

            return prompt

        elif gen_type == "redef":
            function_signature = kwargs.get(
                "function_signature", f"int {function_name}()"
            )
            assumptions = kwargs.get("assumptions", "")

            prompt += "\n" + DEFAULT_REDEF_PROMPT_C.format(
                function_signature=function_signature, assumptions=assumptions
            )

            prompt += "\n" + DEFAULT_REDEF_RETURN_FORMAT_C.format(
                function_signature=function_signature
            )

            return prompt

        else:
            return f"Generate a C function named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract C code blocks from LLM response."""
        for pattern in CODE_BLOCK_PATTERNS:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize C code by removing comments and standardizing format."""
        # Remove single-line comments
        code = re.sub(SINGLE_LINE_COMMENT_PATTERN, "", code)

        # Remove lines that come inline after a comment like
        code = re.sub(SINGLE_INLINE_COMMENT_PATTERN, "", code)

        # Remove multi-line comments
        code = re.sub(MULTI_LINE_COMMENT_PATTERN, "", code, flags=re.DOTALL)

        # Replace all instances of two or more blank lines with 1
        code = re.sub(EXTRA_BLANK_LINES, "\n", code)

        return code
