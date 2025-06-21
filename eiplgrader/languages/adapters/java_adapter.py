"""Java language adapter."""

import re
from typing import List
from ..base import LanguageAdapter, LanguageConfig


DEFAULT_STUDENT_PERSONA_JAVA = """
Pretend you are an introductory CS student learning Java for the very first time. 
You know basic Java syntax and object-oriented concepts.
"""

DEFAULT_CGBG_PROMPT_JAVA = """
Create a method, called {function_name},
according to the following prompt:

Create a method {function_name} that {student_response}
"""

DEFAULT_REDEF_PROMPT_JAVA = """
Create a method based on the following method signature: {function_signature}
You are given the following assumptions about the arguments:
{assumptions}.
"""

DEFAULT_RETURN_FORMAT_JAVA = """
Include only the method and no additional test cases, code, or comments.
Respond with the code for the method {function_name} in the following format
which has the code wrapped in markdown of a java code block:

```java
public static int {function_name}() {{
    return 0;
}}
```"""

DEFAULT_REDEF_RETURN_FORMAT_JAVA = """
Generate the code only and generate it to be surrounded with markdown of a
java code block. It is very important that you use the provided method name
when generating the code. For example:

```java
{function_signature} {{
    // implementation here
}}
```"""

# Regex patterns for code extraction
CODE_BLOCK_PATTERNS = [
    r"```java\n(.*?)\n```",
    r"```\n(.*?)\n```",
]

# Comment patterns
SINGLE_LINE_COMMENT_PATTERN = r"//.*"
MULTI_LINE_COMMENT_PATTERN = r"/\*.*?\*/"

# Whitespace normalization pattern
EXTRA_BLANK_LINES =  r"\n\s*\n" 


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
        """Generate Java-specific prompt for LLM."""
        
        prompt = DEFAULT_STUDENT_PERSONA_JAVA.strip()
        
        if gen_type == "cgbg":
            prompt += "\n" + DEFAULT_CGBG_PROMPT_JAVA.format(
                function_name=function_name, student_response=student_response
            )
            
            prompt += "\n" + DEFAULT_RETURN_FORMAT_JAVA.format(
                function_name=function_name
            )
            
            return prompt

        elif gen_type == "redef":
            function_signature = kwargs.get(
                "function_signature", f"public static void {function_name}()"
            )
            assumptions = kwargs.get("assumptions", "")

            prompt += "\n" + DEFAULT_REDEF_PROMPT_JAVA.format(
                function_signature=function_signature,
                assumptions=assumptions
            )
            
            prompt += "\n" + DEFAULT_REDEF_RETURN_FORMAT_JAVA.format(
                function_signature=function_signature
            )
            
            return prompt

        else:
            return f"Generate a Java method named {function_name}"

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Java code blocks from LLM response."""
        for pattern in CODE_BLOCK_PATTERNS:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def normalize_code(self, code: str) -> str:
        """Normalize Java code by removing comments and standardizing format."""
        # Remove single-line comments
        code = re.sub(SINGLE_LINE_COMMENT_PATTERN, "", code)

        # Remove multi-line comments
        code = re.sub(MULTI_LINE_COMMENT_PATTERN, "", code, flags=re.DOTALL)

        # Replace all instances of two or more blank lines with 1
        code = re.sub(EXTRA_BLANK_LINES, "\n", code)

        return code
