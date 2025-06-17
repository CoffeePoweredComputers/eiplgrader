"""Go language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class GoAdapter(UnifiedLanguageAdapter):
    """Go language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="go",
            display_name="Go",
            file_extensions=[".go"],
            compile_command=["go", "build"],
            run_command=["go", "run"],
            # Enhanced specification
            code_block_tag="go",
            student_model_template="""Pretend you are an introductory CS student learning Go for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Go syntax including package declarations,
imports, and error handling patterns.""",
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="//",
                comment_multi_start="/*",
                comment_multi_end="*/",
                statement_terminator="",
                indentation_type="tabs",
                indentation_size=4,
            ),
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(func\s+(\w+)\s*\([^)]*\)[^{]*{[^}]*})",
                name_capture_group=2,
                requires_return_type=True,
                supports_overloading=False,
                supports_default_params=False,
            ),
            # Validation
            validation_strategy="parser",
            validation_command=["gofmt", "-e"],
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "package_header": "package main\n",
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
The function should follow Go conventions and be part of the main package.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a Go code block:

```go
package main

func {function_name}(parameters) returnType {{
    // function body
}}
```

If the function needs to handle errors, use Go's idiomatic error handling pattern
with multiple return values.""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
Go code block. It is very important that you use the provided function name
when generating the code and include the package declaration. For example:

```go
package main

func {function_name}({params}) returnType {{
    // function implementation
}}
```

If the function should handle errors, include error as the last return value
following Go conventions.""",
                }
            ),
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
            return f"Generate a Go function {function_name} that {student_response}"
        else:
            return f"Generate a Go function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract Go code blocks
        patterns = [
            r"```go\n(.*?)\n```",
            r"```golang\n(.*?)\n```",
            r"```\n(.*?)\n```",  # Generic code block
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
        # Pattern for Go function definitions
        pattern = r"func\s+(\w+)\s*\([^)]*\)[^{]*\{[^}]*\}"
        lines = code.split("\n")

        for i, line in enumerate(lines):
            match = re.search(r"func\s+(\w+)\s*\(", line)
            if match:
                func_name = match.group(1)
                func_dict = {
                    "name": func_name,
                    "signature": line.strip(),
                    "start_line": i + 1,
                    "code": line.strip(),
                }
                functions.append(func_dict)

        return functions

    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        # Basic Go syntax validation using gofmt
        try:
            import subprocess
            import tempfile
            import os

            with tempfile.NamedTemporaryFile(mode="w", suffix=".go", delete=False) as f:
                f.write(code)
                temp_file = f.name

            try:
                result = subprocess.run(
                    ["gofmt", "-e", temp_file],
                    capture_output=True,
                    text=True,
                    timeout=5,
                )
                if result.returncode == 0:
                    return True, None
                else:
                    return False, result.stderr
            finally:
                os.unlink(temp_file)
        except Exception as e:
            return False, f"Validation error: {e}"

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove comments
        code = re.sub(r"/\*.*?\*/", "", code, flags=re.DOTALL)
        code = re.sub(r"//.*", "", code)
        # Remove extra whitespace
        code = re.sub(r"\s+", " ", code)
        code = code.strip()
        return code
