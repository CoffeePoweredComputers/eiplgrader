"""Python language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides


class PythonAdapter(UnifiedLanguageAdapter):
    """Python language adapter - configuration driven"""
    
    def get_language_spec(self) -> LanguageSpec:
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