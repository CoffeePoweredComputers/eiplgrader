"""Rust language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class RustAdapter(UnifiedLanguageAdapter):
    """Rust language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="rust",
            display_name="Rust",
            file_extensions=[".rs"],
            compile_command=["rustc"],
            run_command=["cargo", "run"],

            # Enhanced specification
            code_block_tag="rust",
            student_model_template="""Pretend you are an introductory CS student learning Rust for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Rust syntax including ownership, borrowing,
lifetimes, and error handling with Result types.""",

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
                definition_regex=r"(fn\s+(\w+)\s*(?:<[^>]*>)?\s*\([^)]*\)[^{]*{[^}]*})",
                name_capture_group=2,
                requires_return_type=True,
                supports_overloading=False,
                supports_default_params=False,
                supports_varargs=False
            ),

            # Validation
            validation_strategy="compiler",
            validation_command=["rustc", "--crate-type", "lib", "--emit", "metadata", "-Z", "no-codegen"],

            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
The function should follow Rust conventions and handle errors using Result<T, E> when appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a Rust code block:

```rust
fn {function_name}(parameters) -> ReturnType {{
    // function body
}}
```

If the function can fail, use Rust's idiomatic error handling pattern with Result<T, E>.
Follow Rust ownership rules and borrowing conventions. Use references (&) when you don't
need to take ownership of parameters.""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
Rust code block. It is very important that you use the provided function name
when generating the code. For example:

```rust
fn {function_name}({params}) -> ReturnType {{
    // function implementation
}}
```

If the function should handle errors, use Result<T, E> as the return type
following Rust conventions. Follow ownership and borrowing rules - use references
when appropriate and avoid unnecessary clones.""",
                    "multiple_versions_note": """Each version should be a complete, valid Rust function that follows
ownership and borrowing rules."""
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
            return f"Generate a Rust function {function_name} that {student_response}"
        else:
            return f"Generate a Rust function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract rust code blocks
        patterns = [
            r'```rust\n(.*?)\n```',
            r'```rs\n(.*?)\n```',
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
        pattern = r"fn\s+(\w+)\s*(?:<[^>]*>)?\s*\([^)]*\)(?:\s*->\s*[^{]*)?\s*\{[^}]*\}"
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
        # Use rustc --check for validation
        try:
            import subprocess
            import tempfile
            import os

            # Create a temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.rs', delete=False) as f:
                f.write(code)
                temp_file = f.name

            try:
                result = subprocess.run(
                    ['rustc', '--crate-type', 'lib', '--emit', 'metadata', '-o', '/dev/null', temp_file],
                    capture_output=True,
                    text=True,
                    timeout=10
                )
                if result.returncode == 0:
                    return True, None
                else:
                    return False, result.stderr
            finally:
                os.unlink(temp_file)
        except Exception as e:
            return False, str(e)

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove comments
        code = re.sub(r'//.*', '', code)  # Single line comments
        code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)  # Multi-line comments
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code
