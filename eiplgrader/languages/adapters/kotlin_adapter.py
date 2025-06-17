"""Kotlin language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class KotlinAdapter(UnifiedLanguageAdapter):
    """Kotlin language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="kotlin",
            display_name="Kotlin",
            file_extensions=[".kt", ".kts"],
            run_command=["kotlin"],
            compile_command=["kotlinc"],

            # Enhanced specification
            code_block_tag="kotlin",
            student_model_template="""Pretend you are an introductory CS student learning Kotlin for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Kotlin syntax including null safety and type inference.""",

            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="//",
                comment_multi_start="/*",
                comment_multi_end="*/",
                statement_terminator="",
                indentation_type="spaces",
                indentation_size=4,
            ),

            # Function patterns (handle both regular and single-expression functions)
            function_patterns=FunctionPatterns(
                definition_regex=r"(fun\s+(\w+)\s*\([^)]*\)(?:\s*:\s*[\w\?]+)?\s*(?:\{[^}]*\}|=.*?)(?=fun\s+\w+\s*\(|$))",
                name_capture_group=2,
                requires_return_type=False,  # Kotlin infers types
                supports_overloading=True,
                supports_default_params=True,
                supports_varargs=True
            ),

            # Validation
            validation_strategy="parser",  # Use basic parser validation

            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
Use idiomatic Kotlin style with proper null safety when appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a kotlin code block:

```kotlin
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
kotlin code block. It is very important that you use the provided function name
when generating the code. Use appropriate Kotlin idioms and null safety.
For example:

```kotlin
fun {function_name}({params}): <return_type> {{
    // implementation
}}
```""",
                    "multiple_versions_note": """Each version should use different Kotlin idioms where appropriate
(e.g., single expression functions, when expressions, null-safe operators)."""
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
            return f"Generate a Kotlin function {function_name} that {student_response}"
        else:
            return f"Generate a Kotlin function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract Kotlin code blocks
        patterns = [
            r'```kotlin\n(.*?)\n```',
            r'```kt\n(.*?)\n```',
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
        # Pattern for Kotlin functions (both regular and single-expression, handle multi-line)
        pattern = r"(fun\s+(\w+)\s*\([^)]*\)(?:\s*:\s*[\w\?]+)?\s*(?:\{.*?\}|=.*?))"

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
        # Basic Kotlin syntax validation using kotlinc
        try:
            import subprocess
            import tempfile
            import os

            # Create temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.kt', delete=False) as tmp:
                tmp.write(code)
                tmp_path = tmp.name

            try:
                result = subprocess.run(
                    ['kotlinc', '-cp', '.', tmp_path],
                    capture_output=True,
                    text=True,
                    timeout=10
                )

                if result.returncode == 0:
                    return True, None
                else:
                    return False, result.stderr
            finally:
                os.unlink(tmp_path)
                # Clean up compiled class files
                class_file = tmp_path.replace('.kt', '.class')
                if os.path.exists(class_file):
                    os.unlink(class_file)

        except Exception as e:
            return False, str(e)

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove single-line comments
        code = re.sub(r'//.*', '', code)
        # Remove multi-line comments
        code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code
