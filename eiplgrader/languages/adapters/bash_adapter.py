"""Bash language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class BashAdapter(LanguageAdapter):
    """Adapter for Bash language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return Bash language configuration."""
        return LanguageConfig(
            name="bash",
            display_name="Bash",
            file_extensions=[".sh"],
            run_command=["bash"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Bash-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning bash scripting for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals in bash. You understand basic shell scripting conventions.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a bash function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

The function should use echo to return values. Arguments are accessed using $1, $2, etc.
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
```
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            # Convert Python-style params to bash description
            param_list = params.split(", ") if params else []
            param_desc = ""
            for i, param in enumerate(param_list, 1):
                param_desc += f"${i} is {param}, "
            param_desc = param_desc.rstrip(", ")

            prompt += f"""
Create a bash function based on the following function name: {function_name}
The function takes {len(param_list)} argument(s): {param_desc}.
You are given the following assumptions about the arguments:
{assumptions}.

Use echo to return the result. Arguments are accessed using $1, $2, etc.
Generate the code only and generate it to be surrounded with markdown of a
bash code block. It is very important that you use the provided function name
when generating the code. For example:

```bash
{function_name}() {{
    # function implementation
    echo "result"
}}
```
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Bash code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```bash\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```bash
        if "```bash" in llm_response:
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split("```bash"))
            )[1:]
            return functions

        # Method 3: Look for function definitions
        func_pattern = r"(\w+\s*\(\)\s*\{.*?\})"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL)
        if func_matches:
            return [match.strip() for match in func_matches]

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Bash syntax."""
        import subprocess

        # Use bash -n to check syntax without executing
        try:
            result = subprocess.run(
                ["bash", "-n"], input=code, capture_output=True, text=True
            )

            if result.returncode == 0:
                return True, None
            else:
                return False, f"Syntax error: {result.stderr}"
        except Exception as e:
            return False, f"Validation error: {str(e)}"
