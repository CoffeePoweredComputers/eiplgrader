"""C++ language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class CppAdapter(UnifiedLanguageAdapter):
    """C++ language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="cpp",
            display_name="C++",
            file_extensions=[".cpp", ".cc", ".cxx"],
            compile_command=["g++", "-std=c++17"],
            run_command=["./a.out"],
            # Enhanced specification
            code_block_tag="cpp",
            student_model_template="""Pretend you are an introductory CS student learning C++ for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic C++ syntax including headers, namespaces,
and the Standard Template Library (STL). You know about references and pointers
but prefer using modern C++ features when appropriate.""",
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
                definition_regex=r"((?:(?:inline|static|extern|virtual|const|constexpr)\s+)*(?:\w+(?:::\w+)*(?:<[^>]+>)?(?:\s*[*&]+)?)\s+(\w+)\s*\([^)]*\)[^{]*{[^}]*})",
                name_capture_group=2,
                requires_return_type=True,
                supports_overloading=True,
                supports_default_params=True,
            ),
            # Validation
            validation_strategy="compiler",
            validation_command=["g++", "-std=c++17", "-fsyntax-only"],
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "headers": """#include <iostream>
#include <vector>
// include other necessary headers""",
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
The function should follow modern C++ conventions (C++17).
If the function works with collections, prefer using STL containers like std::vector,
std::string, std::array over raw arrays and C-style strings.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a C++ code block:

```cpp
#include <iostream>
#include <vector>
// include other necessary headers

{return_type} {function_name}(parameters) {{
    // function body
}}
```

Use appropriate parameter passing (by value, reference, or const reference) based
on the function's requirements.""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
C++ code block. It is very important that you use the provided function name
when generating the code and include necessary headers. For example:

```cpp
#include <iostream>
// include other necessary headers

{return_type} {function_name}({params}) {{
    // function implementation
}}
```

Use modern C++ features and STL containers where appropriate.
Prefer references over pointers for non-nullable parameters.""",
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
            return f"Generate a C++ function {function_name} that {student_response}"
        else:
            return f"Generate a C++ function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract C++ code blocks
        patterns = [
            r"```cpp\n(.*?)\n```",
            r"```c\+\+\n(.*?)\n```",
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
        # Pattern for C++ function definitions (simplified)
        pattern = r"(\w+(?:<[^>]+>)?(?:\s*[*&]+)?\s+(\w+)\s*\([^)]*\)[^{]*\{[^}]*\})"
        lines = code.split("\n")

        for i, line in enumerate(lines):
            match = re.search(r"(\w+(?:<[^>]+>)?)(?:\s*[*&]+)?\s+(\w+)\s*\(", line)
            if match:
                func_name = match.group(2)
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
        # Basic C++ syntax validation using g++
        try:
            import subprocess
            import tempfile
            import os

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".cpp", delete=False
            ) as f:
                f.write(code)
                temp_file = f.name

            try:
                result = subprocess.run(
                    ["g++", "-std=c++17", "-fsyntax-only", temp_file],
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
