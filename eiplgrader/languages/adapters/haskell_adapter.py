"""Haskell language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class HaskellAdapter(UnifiedLanguageAdapter):
    """Haskell language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="haskell",
            display_name="Haskell",
            file_extensions=[".hs"],
            compile_command=["ghc"],
            run_command=None,  # Haskell is compiled, not interpreted
            
            # Enhanced specification
            code_block_tag="haskell",
            student_model_template="""Pretend you are an introductory CS student learning Haskell for the very first
time. You have a rudimentary understanding of functions, recursion, pattern matching,
and basic types. You understand functional programming concepts like pure functions,
immutability, and lazy evaluation.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="--",
                comment_multi_start="{-",
                comment_multi_end="-}",
                statement_terminator="",
                indentation_type="spaces",
                indentation_size=2,
                case_sensitive=True
            ),
            
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"((\w+)\s*::[^\n]+\n\2[^=]*=[^}]+?)(?=\n\w+\s*::|$)",
                name_capture_group=2,
                requires_return_type=True,
                supports_overloading=False,
                supports_default_params=False,
                supports_varargs=False
            ),
            
            # Validation
            validation_strategy="compiler",
            validation_command=["ghc", "-fno-code"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "module_header": "module Test where\n\n",
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
The function should follow Haskell conventions with proper type signatures.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a Haskell code block:

```haskell
{function_name} :: Type -> Type -> ReturnType
{function_name} param1 param2 = expression
```

Use proper Haskell types (Int, Integer, Double, String, [a], etc.).
Include the type signature for the function.
Use pattern matching where appropriate.
Remember that Haskell functions are pure and immutable.""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
Haskell code block. It is very important that you use the provided function name
when generating the code and include the type signature. For example:

```haskell
{function_name} :: Type -> Type -> ReturnType
{function_name} {params} = implementation
```

Use standard Haskell conventions:
- Include type signatures
- Use pattern matching for different cases
- Keep functions pure (no side effects)
- Use recursion instead of loops
- Use guards or if-then-else for conditionals"""
                }
            )
        )



