"""Rust language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig


class RustAdapter(LanguageAdapter):
    """Rust language adapter with 4 core methods."""

    def get_config(self) -> LanguageConfig:
        """Return Rust language configuration."""
        return LanguageConfig(
            name="rust",
            display_name="Rust",
            file_extensions=[".rs"],
            run_command=["cargo", "run"],
            compile_command=["rustc"],
            test_timeout=30
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Rust-specific prompt for LLM."""
        if gen_type == "cgbg":
            return f"""Pretend you are an introductory CS student learning Rust for the very first time. You know basic Rust syntax, ownership, borrowing, and lifetimes.

Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a rust code block:

```rust
fn {function_name}() -> i32 {{
    0
}}
```"""
        
        elif gen_type == "redef":
            function_signature = kwargs.get("function_signature", f"fn {function_name}()")
            assumptions = kwargs.get("assumptions", "")
            
            return f"""Pretend you are an introductory CS student learning Rust for the very first time. You know basic Rust syntax, ownership, borrowing, and lifetimes.

Create a function based on the following function signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
rust code block. It is very important that you use the provided function name
when generating the code. For example:

```rust
{function_signature} {{
    0
}}
```"""
        
        else:
            return f"Generate a Rust function named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Rust code blocks from LLM response."""
        patterns = [
            r"```rust\n(.*?)\n```",
            r"```rs\n(.*?)\n```",
            r"```\n(.*?)\n```",
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]
        
        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize Rust code by removing comments and standardizing format."""
        # Remove single-line comments
        code = re.sub(r'//.*', '', code)
        
        # Remove multi-line comments
        code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)
        
        # Remove empty lines and normalize whitespace
        lines = []
        for line in code.split('\n'):
            stripped = line.strip()
            if stripped:
                lines.append(stripped)
        
        # Join lines and normalize whitespace
        if not lines:
            return ""
        
        normalized = ' '.join(lines)
        normalized = re.sub(r'\s+', ' ', normalized)
        return normalized.strip()
