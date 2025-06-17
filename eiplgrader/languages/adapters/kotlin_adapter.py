"""Kotlin language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class KotlinAdapter(UnifiedLanguageAdapter):
    """Kotlin language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="kotlin",
            display_name="Kotlin",
            file_extensions=[".kt", ".kts"],
            run_command=["kotlin"],
            compile_command=["kotlinc"],
            
            # Enhanced specification
            code_block_tag="kotlin",
            student_model_template="""Pretend you are an introductory CS student learning Kotlin for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Kotlin syntax including null safety and type inference.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="//",
                comment_multi_start="/*",
                comment_multi_end="*/",
                statement_terminator="",
                indentation_type="spaces",
                indentation_size=4,
            ),
            
            # Function patterns (handle both regular and single-expression functions)
            function_patterns=FunctionPatterns(
                definition_regex=r"(fun\s+(\w+)\s*\([^)]*\)(?:\s*:\s*[\w\?]+)?\s*(?:\{[^}]*\}|=.*?)(?=fun\s+\w+\s*\(|$))",
                name_capture_group=2,
                requires_return_type=False,  # Kotlin infers types
                supports_overloading=True,
                supports_default_params=True,
                supports_varargs=True
            ),
            
            # Validation
            validation_strategy="parser",  # Use basic parser validation
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
Use idiomatic Kotlin style with proper null safety when appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a kotlin code block:

```kotlin
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
kotlin code block. It is very important that you use the provided function name
when generating the code. Use appropriate Kotlin idioms and null safety.
For example:

```kotlin
fun {function_name}({params}): <return_type> {{
    // implementation
}}
```""",
                    "multiple_versions_note": """Each version should use different Kotlin idioms where appropriate
(e.g., single expression functions, when expressions, null-safe operators)."""
                }
            )
        )



