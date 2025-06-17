"""OCaml language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class OcamlAdapter(UnifiedLanguageAdapter):
    """OCaml language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="ocaml",
            display_name="OCaml",
            file_extensions=[".ml"],
            compile_command=["ocamlc"],
            run_command=["./a.out"],
            
            # Enhanced specification
            code_block_tag="ocaml",
            student_model_template="""Pretend you are an introductory CS student learning OCaml for the very first
time. You have a rudimentary understanding of functions, pattern matching, let bindings, 
recursion, and basic types. You understand functional programming concepts but are still
learning the syntax and idioms of OCaml.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="(* *)",  # OCaml uses (* *) for comments
                comment_multi_start="(*",
                comment_multi_end="*)",
                statement_terminator=";;",  # For top-level expressions
                indentation_type="spaces",
                indentation_size=2,
            ),
            
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(let\s+(?:rec\s+)?(\w+)\s+.*?=.*?)(?=let\s+(?:rec\s+)?\w+|$)",
                name_capture_group=2,
                requires_return_type=False,  # OCaml infers types
                supports_overloading=False,
                supports_default_params=False,
                supports_varargs=False
            ),
            
            # Validation
            validation_strategy="compiler",
            validation_command=["ocamlc", "-c"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
Use standard OCaml conventions - functions are lowercase with underscores, 
and use pattern matching where appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of an ocaml code block:

```ocaml
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of an
ocaml code block. It is very important that you use the provided function name
when generating the code. Use pattern matching and recursion where appropriate.
For example:

```ocaml
let {function_name} {params} =
  (* implementation *)
```""",
                    "multiple_versions_note": """Each version should use different OCaml idioms or approaches
(e.g., pattern matching vs if-then-else, tail recursion vs regular recursion, etc.)"""
                }
            )
        )



