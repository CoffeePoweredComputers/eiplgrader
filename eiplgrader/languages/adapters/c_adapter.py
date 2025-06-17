"""C language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, HeaderRequirements


class CAdapter(UnifiedLanguageAdapter):
    """C language adapter - configuration driven"""
    
    def get_language_spec(self) -> LanguageSpec:
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