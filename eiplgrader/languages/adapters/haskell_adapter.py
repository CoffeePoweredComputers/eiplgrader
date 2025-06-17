"""Haskell language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class HaskellAdapter(LanguageAdapter):
    """Adapter for Haskell language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return Haskell language configuration."""
        return LanguageConfig(
            name="haskell",
            display_name="Haskell",
            file_extensions=[".hs"],
            compile_command=["ghc"],
            run_command=[],  # Haskell is compiled, not interpreted
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate Haskell-specific prompt for LLM."""

        # Base student model for Haskell
        prompt = """Pretend you are an introductory CS student learning Haskell for the very first
time. You have a rudimentary understanding of functions, recursion, pattern matching,
and basic types. You understand functional programming concepts like pure functions,
immutability, and lazy evaluation.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
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
Remember that Haskell functions are pure and immutable.
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function name: {function_name}
with parameters: {params}
You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of a
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
- Use guards or if-then-else for conditionals
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. Each version should be a complete, valid Haskell function with its type signature.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract Haskell code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```haskell\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```haskell
        if "```haskell" in llm_response:
            functions = list(
                map(
                    lambda x: x.split("```")[0].strip(),
                    llm_response.split("```haskell"),
                )
            )[1:]
            return functions

        # Method 3: Look for function definitions with type signatures
        # Pattern to match type signature followed by function definition
        func_pattern = r"(\w+\s*::[^\n]+\n\w+[^=]*=[^}]+?)(?=\n\w+\s*::|$)"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL | re.MULTILINE)
        if func_matches:
            return [match.strip() for match in func_matches]

        # Method 4: Look for just function definitions without type signatures
        simple_pattern = r"(\w+\s+[^=]+=.*?)(?=\n\w+\s+[^=]+=|$)"
        simple_matches = re.findall(simple_pattern, llm_response, re.DOTALL)
        if simple_matches:
            return [match.strip() for match in simple_matches]

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Haskell syntax using ghc."""
        import subprocess
        import tempfile

        try:
            # Write code to temporary file
            with tempfile.NamedTemporaryFile(mode="w", suffix=".hs", delete=False) as f:
                # Wrap in module for proper validation
                f.write("module Test where\n\n")
                f.write(code)
                temp_path = f.name

            # Use ghc to check syntax (-fno-code flag for syntax only)
            result = subprocess.run(
                ["ghc", "-fno-code", temp_path], capture_output=True, text=True
            )

            # Clean up
            import os

            os.unlink(temp_path)

            if result.returncode == 0:
                return True, None
            else:
                # Extract meaningful error messages
                error_lines = result.stderr.strip().split("\n")
                error_msg = "\n".join(
                    [
                        line
                        for line in error_lines
                        if line and not line.startswith("[") and temp_path not in line
                    ]
                )
                return False, f"Syntax error: {error_msg}"

        except FileNotFoundError:
            # ghc not available, skip validation
            return True, None
        except Exception as e:
            return False, f"Validation error: {str(e)}"
