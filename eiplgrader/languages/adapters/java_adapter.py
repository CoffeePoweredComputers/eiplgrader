"""Java language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig


class JavaAdapter(LanguageAdapter):
    """Java language adapter with 4 core methods."""

    def get_config(self) -> LanguageConfig:
        """Return Java language configuration."""
        return LanguageConfig(
            name="java",
            display_name="Java",
            file_extensions=[".java"],
            run_command=["java"],
            compile_command=["javac"],
            test_timeout=30
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Java-specific prompt for LLM."""
        if gen_type == "cgbg":
            return f"""Pretend you are an introductory CS student learning Java for the very first time. You know basic Java syntax and object-oriented concepts.

Create a method, called {function_name},
according to the following prompt:

Create a method {function_name} that {student_response}

Include only the method and no additional test cases, code, or comments.
Respond with the code for the method {function_name} in the following format
which has the code wrapped in markdown of a java code block:

```java
public static int {function_name}() {{
    return 0;
}}
```"""
        
        elif gen_type == "redef":
            function_signature = kwargs.get("function_signature", f"public static void {function_name}()")
            assumptions = kwargs.get("assumptions", "")
            
            return f"""Pretend you are an introductory CS student learning Java for the very first time. You know basic Java syntax and object-oriented concepts.

Create a method based on the following method signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
java code block. It is very important that you use the provided method name
when generating the code. For example:

```java
{function_signature} {{
    // implementation here
}}
```"""
        
        else:
            return f"Generate a Java method named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Java code blocks from LLM response."""
        patterns = [
            r"```java\n(.*?)\n```",
            r"```\n(.*?)\n```",
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]
        
        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize Java code by removing comments and standardizing format."""
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
