"""PHP language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class PhpAdapter(LanguageAdapter):
    """Adapter for PHP language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return PHP language configuration."""
        return LanguageConfig(
            name="php",
            display_name="PHP",
            file_extensions=[".php"],
            run_command=["php"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate PHP-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning PHP for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You don't know about advanced PHP features like namespaces, traits, or type declarations.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
DO NOT use echo or print statements to output the result - use return instead.
Always include the <?php tag at the beginning.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a PHP code block:

```php
<?php
<code here>
```
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function name: function {function_name}({params}) {{
    // function body
}}. You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
PHP code block. It is very important that you use the provided function name
when generating the code. Always include the <?php tag. DO NOT use echo or print statements - use return instead.
For example:

```php
<?php
function {function_name}({params}) {{
    // function implementation
}}
```
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. Each version should be in its own code block.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract PHP code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```php\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            # Ensure each match has <?php tag
            return [self._ensure_php_tag(match.strip()) for match in matches]

        # Method 2: If no markdown blocks, split by ```php
        if "```php" in llm_response:
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split("```php"))
            )[1:]
            return [self._ensure_php_tag(func) for func in functions]

        # Method 3: Look for function definitions
        func_pattern = r"(<\?php\s+)?(function\s+\w+\s*\([^)]*\)\s*\{.*?\n\})"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL | re.MULTILINE)
        if func_matches:
            # func_matches is a list of tuples (php_tag, function_code)
            return [self._ensure_php_tag(match[1].strip()) for match in func_matches]

        # If no code blocks found, return the entire response (ensure it has PHP tag)
        return (
            [self._ensure_php_tag(llm_response.strip())] if llm_response.strip() else []
        )

    def _ensure_php_tag(self, code: str) -> str:
        """Ensure code starts with <?php tag."""
        code = code.strip()
        if not code.startswith("<?php"):
            code = "<?php\n" + code
        return code

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate PHP syntax using php -l."""
        import subprocess
        import tempfile
        import os

        # Ensure code has PHP tag
        code = self._ensure_php_tag(code)

        try:
            # Write code to temporary file
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".php", delete=False
            ) as f:
                f.write(code)
                temp_file = f.name

            # Run php -l (lint) to check syntax
            result = subprocess.run(
                ["php", "-l", temp_file], capture_output=True, text=True
            )

            # Clean up
            os.unlink(temp_file)

            if result.returncode == 0:
                return True, None
            else:
                # Extract error message
                error_msg = result.stdout + result.stderr
                return False, f"Syntax error: {error_msg}"

        except Exception as e:
            return False, f"Validation error: {str(e)}"
