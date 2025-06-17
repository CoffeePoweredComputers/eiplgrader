"""Java language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides


class JavaAdapter(UnifiedLanguageAdapter):
    """Java language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="java",
            display_name="Java",
            file_extensions=[".java"],
            run_command=["java"],
            compile_command=["javac"],
            # Enhanced specification
            code_block_tag="java",
            student_model_template="You understand basic Java syntax including classes, static methods, and types.",
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(public\s+class\s+\w+\s*\{[^}]*\})",
                name_capture_group=0,
                requires_return_type=True,
            ),
            # Validation
            validation_strategy="compiler",
            validation_command=["javac", "-Xlint"],
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "function_declaration_type": "static method",
                    "function_example": """public class Solution {{
    public static <return_type> {function_name}(<parameters>) {{
        // implementation
    }}
}}""",
                    "language_specific_instructions": """Create a static method, called {function_name}, within a Solution class.
Include only the method within a Solution class and no additional test cases, code, or comments.
Use idiomatic Java style with proper type declarations.
Respond with the code for the method {function_name} in the following format
which has the code wrapped in markdown of a java code block:

```java
public class Solution {{
    public static <return_type> {function_name}(<parameters>) {{
        // implementation
    }}
}}
```""",
                    "signature_preservation_note": """It is very important that you use the provided method name
when generating the code. The method must be static and placed within a Solution class.""",
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
            return f"Generate a Java method {function_name} that {student_response}"
        else:
            return f"Generate a Java method named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract Java code blocks
        patterns = [r"```java\n(.*?)\n```", r"```\n(.*?)\n```"]  # Generic code block

        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return the response as-is
        return [llm_response.strip()] if llm_response.strip() else []

    def _extract_functions_impl(self, code: str) -> List[Dict[str, Any]]:
        """Implementation method for function extraction."""
        functions = []
        # Pattern for Java method definitions (simplified)
        lines = code.split("\n")

        for i, line in enumerate(lines):
            # Look for method definitions
            match = re.search(
                r"(public|private|protected)?\s*(static)?\s*\w+\s+(\w+)\s*\(", line
            )
            if match:
                func_name = match.group(3)
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
        # Basic Java syntax validation using javac
        try:
            import subprocess
            import tempfile
            import os

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".java", delete=False
            ) as f:
                f.write(code)
                temp_file = f.name

            try:
                result = subprocess.run(
                    ["javac", "-Xlint", temp_file],
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
