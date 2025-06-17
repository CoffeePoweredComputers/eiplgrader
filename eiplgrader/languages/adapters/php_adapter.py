"""PHP language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class PhpAdapter(UnifiedLanguageAdapter):
    """PHP language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="php",
            display_name="PHP",
            file_extensions=[".php"],
            run_command=["php"],
            
            # Enhanced specification
            code_block_tag="php",
            student_model_template="""Pretend you are an introductory CS student learning PHP for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You don't know about advanced PHP features like namespaces, traits, or type declarations.""",
            
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
                definition_regex=r"(function\s+(\w+)\s*\([^)]*\)\s*\{[^}]*\})",
                name_capture_group=2,
                requires_return_type=False,
                supports_overloading=False,
                supports_default_params=True,
                supports_varargs=True
            ),
            
            # Validation
            validation_strategy="parser",
            validation_command=["php", "-l"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "php_tag": "<?php\n",
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
DO NOT use echo or print statements to output the result - use return instead.
Always include the <?php tag at the beginning.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a PHP code block:

```php
<?php
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
PHP code block. It is very important that you use the provided function name
when generating the code. Always include the <?php tag. DO NOT use echo or print statements - use return instead.
For example:

```php
<?php
function {function_name}({params}) {{
    // function implementation
}}
```"""
                }
            )
        )



