"""Ruby language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class RubyAdapter(UnifiedLanguageAdapter):
    """Ruby language adapter - configuration driven"""

    def get_language_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="ruby",
            display_name="Ruby",
            file_extensions=[".rb"],
            run_command=["ruby"],
            
            # Enhanced specification
            code_block_tag="ruby",
            student_model_template="""Pretend you are an introductory CS student learning Ruby for the very first
time. You have a rudimentary understanding of methods, loops, variables, and
conditionals. You understand Ruby idioms like blocks and symbols.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="#",
                comment_multi_start="=begin",
                comment_multi_end="=end",
                statement_terminator="",
                indentation_type="spaces",
                indentation_size=2,
                block_start="do",
                block_end="end"
            ),
            
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(def\s+(\w+)\s*\([^)]*\).*?end)",
                name_capture_group=2,
                requires_return_type=False,
                supports_overloading=False,
                supports_default_params=True,
                supports_varargs=True
            ),
            
            # Validation
            validation_strategy="parser",
            validation_command=["ruby", "-c"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "method_terminology": "method",  # Ruby uses 'method' instead of 'function'
                    "cgbg_instructions": """Include only the method and no additional test cases, code, or comments.
Respond with the code for the method {function_name} in the following format
which has the code wrapped in markdown of a ruby code block:

```ruby
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
ruby code block. it is very important that you use the provided method name
when generating the code. For example:

```ruby
def {function_name}({params})
  # implementation
end
```"""
                }
            )
        )



