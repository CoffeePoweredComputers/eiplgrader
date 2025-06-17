"""JavaScript language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides


class JavascriptAdapter(UnifiedLanguageAdapter):
    """JavaScript language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="javascript",
            display_name="JavaScript",
            file_extensions=[".js"],
            run_command=["node"],

            # Enhanced specification
            code_block_tag="javascript",
            student_model_template="You are familiar with ES6+ syntax including arrow functions and async/await.",

            # Function patterns (handle both regular and arrow functions)
            function_patterns=FunctionPatterns(
                definition_regex=r"((function\s+\w+\s*\([^)]*\)\s*{[^}]*})|((?:const|let|var)\s+\w+\s*=\s*(?:\([^)]*\)|[^=>\s]+)\s*=>\s*(?:{[^}]*}|[^;]+);?)|(async\s+function\s+\w+\s*\([^)]*\)\s*{[^}]*}))",
                name_capture_group=1
            ),

            # Validation
            validation_strategy="parser",
            validation_command=["node", "--check"],

            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "function_example": "<code here>",
                    "language_specific_instructions": """Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a javascript code block:

```javascript
<code here>
```""",
                    "additional_conventions": """You may use either function declaration or arrow function syntax:

```javascript
function {function_name}({params}) {{
    // implementation here
}}
```

Or:

```javascript
const {function_name} = ({params}) => {{
    // implementation here
}};
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
            return f"Generate a JavaScript function {function_name} that {student_response}"
        else:
            return f"Generate a JavaScript function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract JavaScript code blocks
        patterns = [
            r'```javascript\n(.*?)\n```',
            r'```js\n(.*?)\n```',
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
        # Pattern for function declarations and arrow functions (handle multi-line)
        patterns = [
            r"(function\s+(\w+)\s*\([^)]*\)\s*\{.*?\})",  # function declarations
            r"((?:const|let|var)\s+(\w+)\s*=\s*(?:\([^)]*\)|[^=>\s]+)\s*=>\s*(?:\{.*?\}|[^;]+);?)",  # arrow functions
            r"(async\s+function\s+(\w+)\s*\([^)]*\)\s*\{.*?\})",  # async functions
        ]

        for pattern in patterns:
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
        # Basic JavaScript syntax validation using node --check
        try:
            import subprocess
            import tempfile
            import os

            # Create temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.js', delete=False) as tmp:
                tmp.write(code)
                tmp_path = tmp.name

            try:
                result = subprocess.run(
                    ['node', '--check', tmp_path],
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
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code
