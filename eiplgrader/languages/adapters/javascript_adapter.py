"""JavaScript language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class JavascriptAdapter(LanguageAdapter):
    """Adapter for JavaScript language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return JavaScript language configuration."""
        return LanguageConfig(
            name="javascript",
            display_name="JavaScript",
            file_extensions=[".js"],
            run_command=["node"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate JavaScript-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning JavaScript for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You are familiar with ES6+ syntax including arrow functions and async/await.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a javascript code block:

```javascript
<code here>
```
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function name: function {function_name}({params}) {{
    // implementation here
}}. You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
javascript code block. it is very important that you use the provided function name
when generating the code. For example:

```javascript
function {function_name}({params}) {{
    // implementation here
}}
```

You may also use arrow function syntax if appropriate:

```javascript
const {function_name} = ({params}) => {{
    // implementation here
}};
```
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. You can use either function declaration or arrow function syntax.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract JavaScript code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```javascript\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```javascript
        if "```javascript" in llm_response:
            functions = list(
                map(
                    lambda x: x.split("```")[0].strip(),
                    llm_response.split("```javascript"),
                )
            )[1:]
            return functions

        # Method 3: Look for function definitions (both regular and arrow functions)
        # Regular function pattern
        func_pattern = r"(function\s+\w+\s*\([^)]*\)\s*{[^}]*})"
        # Arrow function pattern
        arrow_pattern = r"((?:const|let|var)\s+\w+\s*=\s*(?:\([^)]*\)|[^=>\s]+)\s*=>\s*(?:{[^}]*}|[^;]+);?)"
        # Async function pattern
        async_pattern = r"(async\s+function\s+\w+\s*\([^)]*\)\s*{[^}]*})"

        all_patterns = f"({func_pattern}|{arrow_pattern}|{async_pattern})"
        func_matches = re.findall(all_patterns, llm_response, re.DOTALL)

        if func_matches:
            # func_matches returns tuples, we need to extract the non-empty groups
            extracted = []
            for match in func_matches:
                if isinstance(match, tuple):
                    for group in match:
                        if group and group.strip():
                            extracted.append(group.strip())
                            break
                else:
                    extracted.append(match.strip())
            return extracted

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate JavaScript syntax using node --check."""
        import subprocess
        import tempfile
        import os

        # Write code to a temporary file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".js", delete=False) as f:
            f.write(code)
            temp_path = f.name

        try:
            # Use node --check to validate syntax
            result = subprocess.run(
                ["node", "--check", temp_path], capture_output=True, text=True
            )

            if result.returncode == 0:
                return True, None
            else:
                # Extract error message
                error_msg = result.stderr.strip()
                return False, f"Syntax error: {error_msg}"

        except Exception as e:
            return False, f"Validation error: {str(e)}"
        finally:
            # Clean up temp file
            if os.path.exists(temp_path):
                os.remove(temp_path)
