"""OCaml language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class OcamlAdapter(LanguageAdapter):
    """Adapter for OCaml language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return OCaml language configuration."""
        return LanguageConfig(
            name="ocaml",
            display_name="OCaml",
            file_extensions=[".ml"],
            compile_command=["ocamlc"],
            run_command=["./a.out"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate OCaml-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning OCaml for the very first
time. You have a rudimentary understanding of functions, pattern matching, let bindings, 
recursion, and basic types. You understand functional programming concepts but are still
learning the syntax and idioms of OCaml.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include only the function and no additional test cases, code, or comments.
Use standard OCaml conventions - functions are lowercase with underscores, 
and use pattern matching where appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of an ocaml code block:

```ocaml
<code here>
```
"""
        elif gen_type == "redef":
            # Function redefinition
            params = kwargs.get("params", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Create a function based on the following function signature: let {function_name} {params} = 
  (* implementation here *)

You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with markdown of an
ocaml code block. It is very important that you use the provided function name
when generating the code. Use pattern matching and recursion where appropriate.
For example:

```ocaml
let {function_name} {params} =
  (* implementation *)
```
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints. Each version should use different OCaml idioms or approaches
(e.g., pattern matching vs if-then-else, tail recursion vs regular recursion, etc.)
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract OCaml code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```ocaml\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```ocaml
        if "```ocaml" in llm_response:
            functions = list(
                map(lambda x: x.split("```")[0].strip(), llm_response.split("```ocaml"))
            )[1:]
            return functions

        # Method 3: Look for let bindings (OCaml function definitions)
        func_pattern = r"(let\s+(?:rec\s+)?\w+\s+.*?=.*?)(?=let\s+(?:rec\s+)?\w+|$)"
        func_matches = re.findall(func_pattern, llm_response, re.DOTALL)
        if func_matches:
            return [match.strip() for match in func_matches]

        # If no code blocks found, return the entire response
        return [llm_response.strip()] if llm_response.strip() else []

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate OCaml syntax using ocamlc."""
        import tempfile
        import subprocess
        import os

        # Create temporary file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".ml", delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            # Try to compile with ocamlc -c (syntax check only)
            result = subprocess.run(
                ["ocamlc", "-c", temp_file], capture_output=True, text=True, timeout=5
            )

            if result.returncode == 0:
                return True, None
            else:
                # Extract error message
                error_msg = result.stderr.strip()
                if not error_msg:
                    error_msg = result.stdout.strip()
                return False, f"Syntax error: {error_msg}"

        except subprocess.TimeoutExpired:
            return False, "Syntax validation timeout"
        except FileNotFoundError:
            # ocamlc not installed, skip validation
            return True, None
        except Exception as e:
            return False, f"Validation error: {str(e)}"
        finally:
            # Clean up
            if os.path.exists(temp_file):
                os.unlink(temp_file)
            # Also clean up .cmo and .cmi files if created
            base_name = temp_file[:-3]
            for ext in [".cmo", ".cmi"]:
                if os.path.exists(base_name + ext):
                    os.unlink(base_name + ext)
