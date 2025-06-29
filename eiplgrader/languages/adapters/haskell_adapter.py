"""Haskell language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig


DEFAULT_STUDENT_PERSONA_HASKELL = """
Pretend you are an introductory CS student learning Haskell for the very first time. 
You know basic Haskell syntax, pattern matching, and recursion.
"""

DEFAULT_CGBG_PROMPT_HASKELL = """
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}
"""

DEFAULT_REDEF_PROMPT_HASKELL = """
Create a function based on the following function signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.
"""

DEFAULT_RETURN_FORMAT_HASKELL = """
Include only the function and no additional test cases, code, or comments.
Do not include type signatures unless absolutely necessary.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a haskell code block:

```haskell
{function_name} x = x
```"""

DEFAULT_REDEF_RETURN_FORMAT_HASKELL = """
Generate the code only and generate it to be surrounded with markdown of a
haskell code block. It is very important that you use the provided function name
when generating the code. For example:

```haskell
{function_signature}
    -- implementation here
```"""

# Regex patterns for code extraction
CODE_BLOCK_PATTERNS = [
    r"```haskell\n(.*?)\n```",
    r"```hs\n(.*?)\n```",
    r"```\n(.*?)\n```",
]

# Comment patterns
SINGLE_LINE_COMMENT_PATTERN = r"--.*"
MULTI_LINE_COMMENT_PATTERN = r"{-.*?-}"

# Whitespace normalization pattern
EXTRA_BLANK_LINES = r"\n\s*\n"


class HaskellAdapter(LanguageAdapter):
    """Haskell language adapter with 4 core methods."""

    def get_config(self) -> LanguageConfig:
        """Return Haskell language configuration."""
        return LanguageConfig(
            name="haskell",
            display_name="Haskell",
            file_extensions=[".hs"],
            run_command=[],  # Haskell is compiled, no interpreter
            compile_command=["ghc"],
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
        """Generate Haskell-specific prompt for LLM."""

        prompt = DEFAULT_STUDENT_PERSONA_HASKELL.strip()

        if gen_type == "cgbg":
            prompt += "\n" + DEFAULT_CGBG_PROMPT_HASKELL.format(
                function_name=function_name, student_response=student_response
            )

            prompt += "\n" + DEFAULT_RETURN_FORMAT_HASKELL.format(
                function_name=function_name
            )

            return prompt

        elif gen_type == "redef":
            function_signature = kwargs.get("function_signature", f"{function_name} =")
            assumptions = kwargs.get("assumptions", "")

            prompt += "\n" + DEFAULT_REDEF_PROMPT_HASKELL.format(
                function_signature=function_signature, assumptions=assumptions
            )

            prompt += "\n" + DEFAULT_REDEF_RETURN_FORMAT_HASKELL.format(
                function_signature=function_signature
            )

            return prompt

        else:
            return f"Generate a Haskell function named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Haskell code blocks from LLM response."""
        for pattern in CODE_BLOCK_PATTERNS:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize Haskell code by removing comments and standardizing format."""

        # Remove {- -} style block comments
        code = re.sub(MULTI_LINE_COMMENT_PATTERN, "", code, flags=re.DOTALL)

        # Replace all instances of two or more blank lines with 1
        code = re.sub(EXTRA_BLANK_LINES, "\n", code)

        return code
