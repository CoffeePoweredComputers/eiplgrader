"""PHP language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class PhpAdapter(UnifiedLanguageAdapter):
    """PHP language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="php",
            display_name="PHP",
            file_extensions=[".php"],
            run_command=["php"],

            # Enhanced specification
            code_block_tag="php",
            student_model_template="""Pretend you are an introductory CS student learning PHP for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You don't know about advanced PHP features like namespaces, traits, or type declarations.""",

            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="//",
                comment_multi_start="/*",
                comment_multi_end="*/",
                statement_terminator=";",
                indentation_type="spaces",
                indentation_size=4,
            ),

            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(function\s+(\w+)\s*\([^)]*\)\s*\{[^}]*\})",
                name_capture_group=2,
                requires_return_type=False,
                supports_overloading=False,
                supports_default_params=True,
                supports_varargs=True
            ),

            # Validation
            validation_strategy="parser",
            validation_command=["php", "-l"],

            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "php_tag": "<?php\n",
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
DO NOT use echo or print statements to output the result - use return instead.
Always include the <?php tag at the beginning.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a PHP code block:

```php
<?php
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
PHP code block. It is very important that you use the provided function name
when generating the code. Always include the <?php tag. DO NOT use echo or print statements - use return instead.
For example:

```php
<?php
function {function_name}({params}) {{
    // function implementation
}}
```"""
                }
            )
        )

    def _generate_prompt_impl(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Implementation method for prompt generation."""
        # This is a fallback - the spec-based system should handle this
        if gen_type == "cgbg":
            return f"Generate a PHP function {function_name} that {student_response}"
        else:
            return f"Generate a PHP function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract PHP code blocks
        patterns = [
            r'```php\n(.*?)\n```',
            r'```\n(.*?)\n```'  # Generic code block
        ]

        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return the response as-is
        return [llm_response.strip()] if llm_response.strip() else []

    def _extract_functions_impl(self, code: str) -> List[Dict[str, Any]]:
        """Implementation method for function extraction."""
        functions = []
        # Pattern for PHP functions (handle multi-line)
        pattern = r"(function\s+(\w+)\s*\([^)]*\)\s*\{.*?\})"

        matches = re.finditer(pattern, code, re.DOTALL)
        for match in matches:
            func_name = match.group(2)
            start_pos = match.start()
            lines_before = code[:start_pos].count('\n')
            func_dict = {
                'name': func_name,
                'signature': match.group(0).split('\n')[0].strip(),
                'start_line': lines_before + 1,
                'code': match.group(0),
            }
            functions.append(func_dict)

        return functions

    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        # Basic PHP syntax validation using php -l
        try:
            import subprocess
            import tempfile
            import os

            # Create temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.php', delete=False) as tmp:
                tmp.write(code)
                tmp_path = tmp.name

            try:
                result = subprocess.run(
                    ['php', '-l', tmp_path],
                    capture_output=True,
                    text=True,
                    timeout=5
                )

                if result.returncode == 0:
                    return True, None
                else:
                    return False, result.stderr
            finally:
                os.unlink(tmp_path)

        except Exception as e:
            return False, str(e)

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove single-line comments
        code = re.sub(r'//.*', '', code)
        # Remove multi-line comments
        code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)
        # Remove PHP tags if present
        code = re.sub(r'<\?php\s*', '', code)
        code = re.sub(r'\?>\s*', '', code)
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code
