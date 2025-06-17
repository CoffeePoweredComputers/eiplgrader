"""Haskell language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class HaskellAdapter(UnifiedLanguageAdapter):
    """Haskell language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="haskell",
            display_name="Haskell",
            file_extensions=[".hs"],
            compile_command=["ghc"],
            run_command=[],  # Haskell is compiled, not interpreted
            # Enhanced specification
            code_block_tag="haskell",
            student_model_template="""Pretend you are an introductory CS student learning Haskell for the very first
time. You have a rudimentary understanding of functions, recursion, pattern matching,
and basic types. You understand functional programming concepts like pure functions,
immutability, and lazy evaluation.""",
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="--",
                comment_multi_start="{-",
                comment_multi_end="-}",
                statement_terminator="",
                indentation_type="spaces",
                indentation_size=2,
                case_sensitive=True,
            ),
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"((\w+)\s*::[^\n]+\n\2[^=]*=[^}]+?)(?=\n\w+\s*::|$)",
                name_capture_group=2,
                requires_return_type=True,
                supports_overloading=False,
                supports_default_params=False,
                supports_varargs=False,
            ),
            # Validation
            validation_strategy="compiler",
            validation_command=["ghc", "-fno-code"],
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "module_header": "module Test where\n\n",
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
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
Remember that Haskell functions are pure and immutable.""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
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
- Use guards or if-then-else for conditionals""",
                }
            ),
        )

    def _generate_prompt_impl(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Implementation method for prompt generation."""
        # This is a fallback - the spec-based system should handle this
        if gen_type == "cgbg":
            return (
                f"Generate a Haskell function {function_name} that {student_response}"
            )
        else:
            return f"Generate a Haskell function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract Haskell code blocks
        patterns = [
            r"```haskell\n(.*?)\n```",
            r"```hs\n(.*?)\n```",
            r"```\n(.*?)\n```",  # Generic code block
        ]

        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]

        # If no code blocks found, return the response as-is
        return [llm_response.strip()] if llm_response.strip() else []

    def _extract_functions_impl(self, code: str) -> List[Dict[str, Any]]:
        """Implementation method for function extraction."""
        functions = []
        # Pattern for Haskell function definitions with type signatures
        lines = code.split("\n")

        for i, line in enumerate(lines):
            # Look for function definitions (pattern: name :: Type)
            if "::" in line:
                match = re.search(r"^(\w+)\s*::", line.strip())
                if match:
                    func_name = match.group(1)
                    func_dict = {
                        "name": func_name,
                        "signature": line.strip(),
                        "start_line": i + 1,
                        "code": line.strip(),
                    }
                    functions.append(func_dict)
            # Also look for function definitions without type signatures
            elif "=" in line and not line.strip().startswith("--"):
                match = re.search(r"^(\w+)\s+.*=", line.strip())
                if match:
                    func_name = match.group(1)
                    func_dict = {
                        "name": func_name,
                        "signature": line.strip(),
                        "start_line": i + 1,
                        "code": line.strip(),
                    }
                    functions.append(func_dict)

        return functions

    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        # Basic Haskell syntax validation using ghc
        try:
            import subprocess
            import tempfile
            import os

            with tempfile.NamedTemporaryFile(mode="w", suffix=".hs", delete=False) as f:
                f.write(code)
                temp_file = f.name

            try:
                result = subprocess.run(
                    ["ghc", "-fno-code", temp_file],
                    capture_output=True,
                    text=True,
                    timeout=5,
                )
                if result.returncode == 0:
                    return True, None
                else:
                    return False, result.stderr
            finally:
                os.unlink(temp_file)
        except Exception as e:
            return False, f"Validation error: {e}"

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove comments
        code = re.sub(r"\{-.*?-\}", "", code, flags=re.DOTALL)
        code = re.sub(r"--.*", "", code)
        # Remove extra whitespace
        code = re.sub(r"\s+", " ", code)
        code = code.strip()
        return code
