"""Haskell language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig


class HaskellAdapter(LanguageAdapter):
    """Haskell language adapter with 4 core methods."""

    def get_config(self) -> LanguageConfig:
        """Return Haskell language configuration."""
        return LanguageConfig(
            name="haskell",
            display_name="Haskell",
            file_extensions=[".hs"],
            run_command=None,  # Haskell is compiled, no interpreter
            compile_command=["ghc"],
            test_timeout=30
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Haskell-specific prompt for LLM."""
        if gen_type == "cgbg":
            return f"""Pretend you are an introductory CS student learning Haskell for the very first time. You know basic Haskell syntax, pattern matching, and recursion.

Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Do not include type signatures unless absolutely necessary.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a haskell code block:

```haskell
{function_name} x = x
```"""
        
        elif gen_type == "redef":
            function_signature = kwargs.get("function_signature", f"{function_name} =")
            assumptions = kwargs.get("assumptions", "")
            
            return f"""Pretend you are an introductory CS student learning Haskell for the very first time. You know basic Haskell syntax, pattern matching, and recursion.

Create a function based on the following function signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
haskell code block. It is very important that you use the provided function name
when generating the code. For example:

```haskell
{function_signature}
    -- implementation here
```"""
        
        else:
            return f"Generate a Haskell function named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Haskell code blocks from LLM response."""
        patterns = [
            r"```haskell\n(.*?)\n```",
            r"```hs\n(.*?)\n```",
            r"```\n(.*?)\n```",
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]
        
        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize Haskell code by removing comments and standardizing format."""
        lines = []
        
        # Process each line
        for line in code.split('\n'):
            # Remove single-line comments (-- comment)
            if '--' in line:
                # Handle string literals that might contain --
                in_string = False
                comment_start = -1
                i = 0
                while i < len(line):
                    if line[i] == '"' and (i == 0 or line[i-1] != '\\'):
                        in_string = not in_string
                    elif not in_string and i < len(line) - 1 and line[i:i+2] == '--':
                        comment_start = i
                        break
                    i += 1
                
                if comment_start >= 0:
                    line = line[:comment_start]
            
            # Skip empty lines and whitespace-only lines
            stripped = line.strip()
            if stripped:
                lines.append(stripped)
        
        # Join lines and normalize whitespace
        if not lines:
            return ""
        
        # For multi-line functions, preserve some structure
        normalized = ' '.join(lines)
        
        # Remove multiple spaces but preserve structure around operators
        normalized = re.sub(r'\s+', ' ', normalized)
        
        # Remove {- -} style block comments
        normalized = re.sub(r'{-.*?-}', '', normalized, flags=re.DOTALL)
        
        return normalized.strip()