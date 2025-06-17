"""Java language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class JavaAdapter(LanguageAdapter):
    """Adapter for Java language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return Java language configuration."""
        return LanguageConfig(
            name="java",
            display_name="Java",
            file_extensions=[".java"],
            run_command=["java"],
            compile_command=["javac"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Java-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning Java for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Java syntax including classes, static methods, and types.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a static method, called {function_name}, within a Solution class,
according to the following prompt:

Create a method {function_name} that {student_response}

Include only the method within a Solution class and no additional test cases, code, or comments.
Use idiomatic Java style with proper type declarations.
Respond with the code for the method {function_name} in the following format
which has the code wrapped in markdown of a java code block:

```java
public class Solution {{
    public static <return_type> {function_name}(<parameters>) {{
        // implementation
    }}
}}
```
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a static method based on the following method signature: public static <return_type> {function_name}({params})
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
java code block. It is very important that you use the provided method name
when generating the code. The method must be static and placed within a Solution class.
For example:

```java
public class Solution {{
    public static <return_type> {function_name}({params}) {{
        // implementation
    }}
}}
```
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this method with these formatting
constraints. Each version should use different Java idioms where appropriate
(e.g., enhanced for loops vs traditional loops, ternary operators vs if-else).
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Java code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```java\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```java
        if "```java" in llm_response:
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split("```java"))
            )[1:]
            return functions

        # Method 3: Look for class definitions with methods
        class_pattern = r"(public\s+class\s+\w+\s*\{[^}]*\})"
        class_matches = re.findall(class_pattern, llm_response, re.DOTALL)
        if class_matches:
            return [match.strip() for match in class_matches]

        # Method 4: Look for standalone method definitions
        method_pattern = r"(public\s+static\s+\w+\s+\w+\s*\([^)]*\)\s*\{[^}]*\})"
        method_matches = re.findall(method_pattern, llm_response, re.DOTALL)
        if method_matches:
            # Wrap standalone methods in Solution class
            wrapped_methods = []
            for method in method_matches:
                wrapped = f"""public class Solution {{
    {method}
}}"""
                wrapped_methods.append(wrapped)
            return wrapped_methods

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Java syntax using basic checks."""
        # Basic syntax validation
        # Check for balanced braces
        brace_count = code.count("{") - code.count("}")
        if brace_count != 0:
            return False, f"Unbalanced braces: {brace_count} more '{{' than '}}'"

        # Check for balanced parentheses
        paren_count = code.count("(") - code.count(")")
        if paren_count != 0:
            return False, f"Unbalanced parentheses: {paren_count} more '(' than ')'"

        # Check for class definition
        if not re.search(r"(public\s+)?class\s+\w+", code):
            return False, "No class definition found"

        # Check for method definition
        if not re.search(r"(public\s+)?(static\s+)?\w+\s+\w+\s*\(", code):
            return False, "No method definition found"

        # Basic checks passed
        return True, None
