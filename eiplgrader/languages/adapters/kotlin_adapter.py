"""Kotlin language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class KotlinAdapter(LanguageAdapter):
    """Adapter for Kotlin language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return Kotlin language configuration."""
        return LanguageConfig(
            name="kotlin",
            display_name="Kotlin",
            file_extensions=[".kt", ".kts"],
            run_command=["kotlin"],
            compile_command=["kotlinc"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Kotlin-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning Kotlin for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Kotlin syntax including null safety and type inference.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Use idiomatic Kotlin style with proper null safety when appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a kotlin code block:

```kotlin
<code here>
```
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function signature: fun {function_name}({params}): <return_type>
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
kotlin code block. It is very important that you use the provided function name
when generating the code. Use appropriate Kotlin idioms and null safety.
For example:

```kotlin
fun {function_name}({params}): <return_type> {{
    // implementation
}}
```
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. Each version should use different Kotlin idioms where appropriate
(e.g., single expression functions, when expressions, null-safe operators).
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Kotlin code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```kotlin\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```kotlin
        if "```kotlin" in llm_response:
            functions = list(
                map(
                    lambda x: x.split("```")[0].strip(), llm_response.split("```kotlin")
                )
            )[1:]
            return functions

        # Method 3: Look for function definitions (both regular and single-expression)
        # Match regular functions: fun name(...): Type { ... }
        # Match single-expression functions: fun name(...) = expression
        func_pattern = r"(fun\s+\w+\s*\([^)]*\)(?:\s*:\s*[\w\?]+)?\s*(?:\{[^}]*\}|=.*?)(?=fun\s+\w+\s*\(|$))"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL)
        if func_matches:
            return [match.strip() for match in func_matches]

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Kotlin syntax using basic checks."""
        # Basic syntax validation
        # Check for balanced braces
        brace_count = code.count("{") - code.count("}")
        if brace_count != 0:
            return False, f"Unbalanced braces: {brace_count} more '{{' than '}}'"

        # Check for balanced parentheses
        paren_count = code.count("(") - code.count(")")
        if paren_count != 0:
            return False, f"Unbalanced parentheses: {paren_count} more '(' than ')'"

        # Check for function definition
        if not re.search(r"fun\s+\w+\s*\(", code):
            return False, "No function definition found"

        # Basic checks passed
        return True, None
