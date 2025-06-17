"""Bash language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class BashAdapter(UnifiedLanguageAdapter):
    """Bash language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="bash",
            display_name="Bash",
            file_extensions=[".sh"],
            run_command=["bash"],

            # Enhanced specification
            code_block_tag="bash",
            student_model_template="""Pretend you are an introductory CS student learning bash scripting for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals in bash. You understand basic shell scripting conventions.""",

            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="#",
                statement_terminator="",
                indentation_type="spaces",
                indentation_size=4,
                block_start="{",
                block_end="}"
            ),

            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"((\w+)\s*\(\)\s*\{[^}]*\})",
                name_capture_group=2,
                requires_return_type=False,
                supports_overloading=False,
                supports_default_params=False,
                supports_varargs=True  # Bash functions can accept any number of args
            ),

            # Validation
            validation_strategy="parser",
            validation_command=["bash", "-n"],

            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """The function should use echo to return values. Arguments are accessed using $1, $2, etc.
Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a bash code block:

```bash
<code here>
```

Example format:
```bash
{function_name}() {{
    # function body here
    echo "result"
}}
```""",
                    "redef_instructions": """Use echo to return the result. Arguments are accessed using $1, $2, etc.
Generate the code only and generate it to be surrounded with markdown of a
bash code block. It is very important that you use the provided function name
when generating the code. For example:

```bash
{function_name}() {{
    # function implementation
    echo "result"
}}
```""",
                    "bash_params_note": "Arguments are accessed using $1, $2, etc."
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
            return f"Generate a bash function {function_name} that {student_response}"
        else:
            return f"Generate a bash function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract bash code blocks
        patterns = [
            r'```bash\n(.*?)\n```',
            r'```sh\n(.*?)\n```',
            r'```shell\n(.*?)\n```',
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
        pattern = r"(\w+)\s*\(\)\s*\{[^}]*\}"
        lines = code.split('\n')

        for i, line in enumerate(lines):
            match = re.search(pattern, line)
            if match:
                func_name = match.group(1)
                func_dict = {
                    'name': func_name,
                    'signature': line.strip(),
                    'start_line': i + 1,
                    'code': match.group(0),
                }
                functions.append(func_dict)

        return functions

    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        # Basic bash syntax validation
        try:
            import subprocess
            result = subprocess.run(
                ['bash', '-n'],
                input=code,
                text=True,
                capture_output=True,
                timeout=5
            )
            if result.returncode == 0:
                return True, None
            else:
                return False, result.stderr
        except Exception as e:
            return False, str(e)

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove comments
        code = re.sub(r'#.*', '', code)
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code
