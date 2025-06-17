"""Go language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class GoAdapter(UnifiedLanguageAdapter):
    """Go language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
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
                supports_default_params=False
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
following Go conventions."""
                }
            )
        )



