"""C language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, HeaderRequirements


class CAdapter(UnifiedLanguageAdapter):
    """C language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="c",
            display_name="C",
            file_extensions=[".c"],
            compile_command=["gcc"],
            run_command=None,

            # Enhanced specification
            code_block_tag="c",
            student_model_template="You understand basic C syntax including headers, pointers, and memory management patterns.",

            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(\w+\s+\w+\s*\([^)]*\)[^{]*{[^}]*})",
                name_capture_group=1
            ),

            # Header requirements
            header_requirements=HeaderRequirements(
                required_imports=["#include <stdio.h>", "#include <stdlib.h>"]
            ),

            # Validation
            validation_strategy="compiler",
            validation_command=["gcc", "-fsyntax-only"],
            requires_main_wrapper=True,

            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "function_example": "returnType {function_name}(parameters) {\n    // function body\n}",
                    "language_specific_instructions": """The function should follow C conventions.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a C code block:

```c
#include <stdio.h>
#include <stdlib.h>

returnType {function_name}(parameters) {{
    // function body
}}
```

Include necessary headers like stdio.h and stdlib.h if needed.
Use proper C types (int, char, float, double, etc.).
If working with arrays, remember C arrays are passed as pointers.""",
                    "language_specific_conventions": """Use standard C conventions for parameter passing:
- Pass by value for basic types
- Pass pointers for arrays or when modifications are needed
- Use const for read-only pointer parameters"""
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
            return f"Generate a C function {function_name} that {student_response}"
        else:
            return f"Generate a C function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract C code blocks
        patterns = [
            r'```c\n(.*?)\n```',
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
        # Pattern for C function definitions
        pattern = r"(\w+\s+\w+\s*\([^)]*\)[^{]*\{[^}]*\})"
        lines = code.split('\n')

        for i, line in enumerate(lines):
            match = re.search(r"(\w+)\s+(\w+)\s*\(", line)
            if match:
                func_name = match.group(2)
                func_dict = {
                    'name': func_name,
                    'signature': line.strip(),
                    'start_line': i + 1,
                    'code': line.strip(),
                }
                functions.append(func_dict)

        return functions

    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        # Basic C syntax validation using gcc
        try:
            import subprocess
            import tempfile
            import os

            with tempfile.NamedTemporaryFile(mode='w', suffix='.c', delete=False) as f:
                f.write(code)
                temp_file = f.name

            try:
                result = subprocess.run(
                    ['gcc', '-fsyntax-only', temp_file],
                    capture_output=True,
                    text=True,
                    timeout=5
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
        code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)
        code = re.sub(r'//.*', '', code)
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code
