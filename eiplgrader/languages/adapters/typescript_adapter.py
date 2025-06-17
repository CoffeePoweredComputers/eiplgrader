"""TypeScript language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class TypescriptAdapter(UnifiedLanguageAdapter):
    """TypeScript language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="typescript",
            display_name="TypeScript",
            file_extensions=[".ts"],
            run_command=["ts-node"],
            compile_command=["tsc"],
            
            # Enhanced specification
            code_block_tag="typescript",
            student_model_template="""Pretend you are an introductory CS student learning TypeScript for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You are familiar with TypeScript type annotations and basic type safety concepts.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="//",
                comment_multi_start="/*",
                comment_multi_end="*/",
                statement_terminator=";",
                indentation_type="spaces",
                indentation_size=2,
            ),
            
            # Function patterns (handle multiple function declaration styles)
            function_patterns=FunctionPatterns(
                definition_regex=r"((function\s+\w+\s*(?:<[^>]+>)?\s*\([^)]*\)\s*(?::\s*\w+(?:<[^>]+>)?)?\s*{[^}]*})|((?:const|let|var)\s+\w+\s*(?::\s*[^=]+)?\s*=\s*(?:<[^>]+>)?\s*(?:\([^)]*\)|[^=>\s]+)\s*(?::\s*\w+(?:<[^>]+>)?)?\s*=>\s*(?:{[^}]*}|[^;]+);?)|(async\s+function\s+\w+\s*(?:<[^>]+>)?\s*\([^)]*\)\s*:\s*(?:Promise<[^>]+>|\w+)\s*{[^}]*}))",
                name_capture_group=1,
                requires_return_type=False,  # TypeScript infers types
                supports_overloading=True,
                supports_default_params=True,
                supports_varargs=True
            ),
            
            # Validation
            validation_strategy="compiler",
            validation_command=["tsc", "--noEmit", "--allowJs", "--checkJs"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """Include type annotations for all parameters and the return type.
Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a typescript code block:

```typescript
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
typescript code block. It is very important that you use the provided function name
when generating the code and include appropriate type annotations. For example:

```typescript
function {function_name}({params}): returnType {{
    // implementation here
}}
```

You may also use arrow function syntax with types if appropriate:

```typescript
const {function_name} = ({params}): returnType => {{
    // implementation here
}};
```""",
                    "multiple_versions_note": """You can use either function declaration or arrow function syntax.
Always include proper TypeScript type annotations."""
                }
            )
        )



