"""Python language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class PythonAdapter(LanguageAdapter):
    """Adapter for Python language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return Python language configuration."""
        return LanguageConfig(
            name="python",
            display_name="Python",
            file_extensions=[".py"],
            run_command=["python3"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Python-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning python for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You also don't know about type annotations.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a python code block:

```python
<code here>
```
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function name: def {function_name}({params}):
pass. You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
python code block. it is very important that you use the provided function name
when generating the code. For example:

```python
def {function_name}({params}):
    pass
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
        """Extract Python code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```python\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```python
        if "```python" in llm_response:
            functions = list(
                map(
                    lambda x: x.split("```")[0].strip(), llm_response.split("```python")
                )
            )[1:]
            return functions

        # Method 3: Look for function definitions
        func_pattern = r"(def\s+\w+\s*\([^)]*\):.*?)(?=def\s+\w+\s*\(|$)"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL)
        if func_matches:
            return [match.strip() for match in func_matches]

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Python syntax."""
        try:
            compile(code, "<string>", "exec")
            return True, None
        except SyntaxError as e:
            return False, f"Syntax error: {str(e)}"
        except Exception as e:
            return False, f"Validation error: {str(e)}"
