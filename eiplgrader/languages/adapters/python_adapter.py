"""Python language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
import ast
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides


class PythonAdapter(UnifiedLanguageAdapter):
    """Python language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="python",
            display_name="Python",
            file_extensions=[".py"],
            run_command=["python3"],

            # Enhanced specification
            code_block_tag="python",
            student_model_template="You also don't know about type annotations.",

            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(def\s+\w+\s*\([^)]*\):.*?)(?=def\s+\w+\s*\(|$)",
                name_capture_group=1,
                supports_default_params=True,
                supports_varargs=True
            ),

            # Validation
            validation_strategy="parser",

            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "function_example": "<code here>",
                    "language_specific_instructions": "Respond with the code for the function {function_name} in the following format which has the code wrapped in markdown of a python code block:\n\n```python\n<code here>\n```"
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
            return f"Generate a Python function {function_name} that {student_response}"
        else:
            return f"Generate a Python function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract Python code blocks
        patterns = [
            r'```python\n(.*?)\n```',
            r'```py\n(.*?)\n```',
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
        try:
            tree = ast.parse(code)
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    func_dict = {
                        'name': node.name,
                        'signature': f"def {node.name}(...)",
                        'start_line': node.lineno,
                        'end_line': getattr(node, 'end_lineno', node.lineno),
                        'code': ast.get_source_segment(code, node) or f"def {node.name}(...): ..."
                    }
                    functions.append(func_dict)
        except SyntaxError:
            # Fallback to regex if AST parsing fails
            pattern = r"def\s+(\w+)\s*\([^)]*\):"
            lines = code.split('\n')
            for i, line in enumerate(lines):
                match = re.search(pattern, line)
                if match:
                    func_name = match.group(1)
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
        try:
            ast.parse(code)
            return True, None
        except SyntaxError as e:
            return False, str(e)
        except Exception as e:
            return False, str(e)

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove comments and docstrings
        lines = code.split('\n')
        normalized_lines = []
        in_multiline_comment = False

        for line in lines:
            # Remove single-line comments
            if '#' in line:
                line = line[:line.index('#')]

            # Skip empty lines
            line = line.strip()
            if line:
                normalized_lines.append(line)

        # Join and normalize whitespace
        code = ' '.join(normalized_lines)
        code = re.sub(r'\s+', ' ', code)
        return code.strip()
