"""Rust language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class RustAdapter(UnifiedLanguageAdapter):
    """Rust language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="rust",
            display_name="Rust",
            file_extensions=[".rs"],
            compile_command=["rustc"],
            run_command=["cargo", "run"],
            
            # Enhanced specification
            code_block_tag="rust",
            student_model_template="""Pretend you are an introductory CS student learning Rust for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You understand basic Rust syntax including ownership, borrowing,
lifetimes, and error handling with Result types.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="//",
                comment_multi_start="/*",
                comment_multi_end="*/",
                statement_terminator=";",
                indentation_type="spaces",
                indentation_size=4,
            ),
            
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(fn\s+(\w+)\s*(?:<[^>]*>)?\s*\([^)]*\)[^{]*{[^}]*})",
                name_capture_group=2,
                requires_return_type=True,
                supports_overloading=False,
                supports_default_params=False,
                supports_varargs=False
            ),
            
            # Validation
            validation_strategy="compiler",
            validation_command=["rustc", "--crate-type", "lib", "--emit", "metadata", "-Z", "no-codegen"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
The function should follow Rust conventions and handle errors using Result<T, E> when appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a Rust code block:

```rust
fn {function_name}(parameters) -> ReturnType {{
    // function body
}}
```

If the function can fail, use Rust's idiomatic error handling pattern with Result<T, E>.
Follow Rust ownership rules and borrowing conventions. Use references (&) when you don't
need to take ownership of parameters.""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
Rust code block. It is very important that you use the provided function name
when generating the code. For example:

```rust
fn {function_name}({params}) -> ReturnType {{
    // function implementation
}}
```

If the function should handle errors, use Result<T, E> as the return type
following Rust conventions. Follow ownership and borrowing rules - use references
when appropriate and avoid unnecessary clones.""",
                    "multiple_versions_note": """Each version should be a complete, valid Rust function that follows
ownership and borrowing rules."""
                }
            )
        )



