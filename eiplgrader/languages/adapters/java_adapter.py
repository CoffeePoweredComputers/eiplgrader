"""Java language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides


class JavaAdapter(UnifiedLanguageAdapter):
    """Java language adapter - configuration driven"""
    
    def get_language_spec(self) -> LanguageSpec:
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
                requires_return_type=True
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
when generating the code. The method must be static and placed within a Solution class."""
                }
            )
        )