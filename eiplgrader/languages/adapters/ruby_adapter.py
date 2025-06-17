"""Ruby language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class RubyAdapter(LanguageAdapter):
    """Adapter for Ruby language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return Ruby language configuration."""
        return LanguageConfig(
            name="ruby",
            display_name="Ruby",
            file_extensions=[".rb"],
            run_command=["ruby"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Ruby-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning Ruby for the very first
time. You have a rudimentary understanding of methods, loops, variables, and
conditionals. You understand Ruby idioms like blocks and symbols.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a method, called {function_name},
according to the following prompt:

Create a method {function_name} that {student_response}

Include only the method and no additional test cases, code, or comments.
Respond with the code for the method {function_name} in the following format
which has the code wrapped in markdown of a ruby code block:

```ruby
<code here>
```
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a method based on the following method name: def {function_name}({params})
end. You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
ruby code block. it is very important that you use the provided method name
when generating the code. For example:

```ruby
def {function_name}({params})
  # implementation
end
```
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this method with these formatting
constraints.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Ruby code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```ruby\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```ruby
        if "```ruby" in llm_response:
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split("```ruby"))
            )[1:]
            return functions

        # Method 3: Look for method definitions
        func_pattern = r"(def\s+\w+\s*\([^)]*\).*?end)"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL | re.MULTILINE)
        if func_matches:
            return [match.strip() for match in func_matches]

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Ruby syntax using ruby -c."""
        import subprocess
        import tempfile
        import os

        # First check if the code looks like it contains a method definition
        # This helps catch cases where someone just types random text
        if not any(
            keyword in code
            for keyword in [
                "def",
                "class",
                "module",
                "do",
                "end",
                "=",
                "(",
                ")",
                "{",
                "}",
            ]
        ):
            # Check if it's just a plain string literal without any Ruby structure
            stripped = code.strip()
            if stripped and not (
                stripped.startswith('"')
                or stripped.startswith("'")
                or stripped.startswith("%")
            ):
                return (
                    False,
                    "Code does not appear to contain valid Ruby method definitions",
                )

        try:
            # Write code to temporary file
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".rb", delete=False
            ) as temp_file:
                temp_file.write(code)
                temp_file_path = temp_file.name

            # Run ruby -c for syntax check
            result = subprocess.run(
                ["ruby", "-c", temp_file_path], capture_output=True, text=True
            )

            # Clean up
            os.unlink(temp_file_path)

            if result.returncode == 0:
                return True, None
            else:
                # Extract error message
                error_msg = result.stderr.strip()
                return False, f"Syntax error: {error_msg}"

        except Exception as e:
            return False, f"Validation error: {str(e)}"
