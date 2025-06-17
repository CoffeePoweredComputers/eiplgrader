"""Ruby language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class RubyAdapter(UnifiedLanguageAdapter):
    """Ruby language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="ruby",
            display_name="Ruby",
            file_extensions=[".rb"],
            run_command=["ruby"],

            # Enhanced specification
            code_block_tag="ruby",
            student_model_template="""Pretend you are an introductory CS student learning Ruby for the very first
time. You have a rudimentary understanding of methods, loops, variables, and
conditionals. You understand Ruby idioms like blocks and symbols.""",

            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="#",
                comment_multi_start="=begin",
                comment_multi_end="=end",
                statement_terminator="",
                indentation_type="spaces",
                indentation_size=2,
                block_start="do",
                block_end="end"
            ),

            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(def\s+(\w+)\s*\([^)]*\).*?end)",
                name_capture_group=2,
                requires_return_type=False,
                supports_overloading=False,
                supports_default_params=True,
                supports_varargs=True
            ),

            # Validation
            validation_strategy="parser",
            validation_command=["ruby", "-c"],

            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "method_terminology": "method",  # Ruby uses 'method' instead of 'function'
                    "cgbg_instructions": """Include only the method and no additional test cases, code, or comments.
Respond with the code for the method {function_name} in the following format
which has the code wrapped in markdown of a ruby code block:

```ruby
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
ruby code block. it is very important that you use the provided method name
when generating the code. For example:

```ruby
def {function_name}({params})
  # implementation
end
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
            return f"Generate a Ruby method {function_name} that {student_response}"
        else:
            return f"Generate a Ruby method named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract Ruby code blocks
        patterns = [
            r'```ruby\n(.*?)\n```',
            r'```rb\n(.*?)\n```',
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
        # Pattern for Ruby methods (handle multi-line, with or without parentheses)
        pattern = r"(def\s+(\w+)(?:\s*\([^)]*\)|\s+[^;\n]*)?.*?end)"

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
        # Basic Ruby syntax validation using ruby -c
        try:
            import subprocess
            import tempfile
            import os

            # Create temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.rb', delete=False) as tmp:
                tmp.write(code)
                tmp_path = tmp.name

            try:
                result = subprocess.run(
                    ['ruby', '-c', tmp_path],
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
        code = re.sub(r'#.*', '', code)
        # Remove multi-line comments
        code = re.sub(r'=begin.*?=end', '', code, flags=re.DOTALL)
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code
