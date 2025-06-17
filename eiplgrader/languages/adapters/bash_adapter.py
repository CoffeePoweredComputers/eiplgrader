"""Bash language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class BashAdapter(UnifiedLanguageAdapter):
    """Bash language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="bash",
            display_name="Bash",
            file_extensions=[".sh"],
            run_command=["bash"],
            
            # Enhanced specification
            code_block_tag="bash",
            student_model_template="""Pretend you are an introductory CS student learning bash scripting for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals in bash. You understand basic shell scripting conventions.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="#",
                statement_terminator="",
                indentation_type="spaces",
                indentation_size=4,
                block_start="{",
                block_end="}"
            ),
            
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"((\w+)\s*\(\)\s*\{[^}]*\})",
                name_capture_group=2,
                requires_return_type=False,
                supports_overloading=False,
                supports_default_params=False,
                supports_varargs=True  # Bash functions can accept any number of args
            ),
            
            # Validation
            validation_strategy="parser",
            validation_command=["bash", "-n"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """The function should use echo to return values. Arguments are accessed using $1, $2, etc.
Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a bash code block:

```bash
<code here>
```

Example format:
```bash
{function_name}() {{
    # function body here
    echo "result"
}}
```""",
                    "redef_instructions": """Use echo to return the result. Arguments are accessed using $1, $2, etc.
Generate the code only and generate it to be surrounded with markdown of a
bash code block. It is very important that you use the provided function name
when generating the code. For example:

```bash
{function_name}() {{
    # function implementation
    echo "result"
}}
```""",
                    "bash_params_note": "Arguments are accessed using $1, $2, etc."
                }
            )
        )



