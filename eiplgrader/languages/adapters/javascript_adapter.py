"""JavaScript language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides


class JavascriptAdapter(UnifiedLanguageAdapter):
    """JavaScript language adapter - configuration driven"""
    
    def get_language_spec(self) -> LanguageSpec:
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