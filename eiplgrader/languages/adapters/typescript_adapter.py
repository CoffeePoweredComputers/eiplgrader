"""TypeScript language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class TypescriptAdapter(LanguageAdapter):
    """Adapter for TypeScript language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return TypeScript language configuration."""
        return LanguageConfig(
            name="typescript",
            display_name="TypeScript",
            file_extensions=[".ts"],
            run_command=["ts-node"],
            compile_command=["tsc"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate TypeScript-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning TypeScript for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You are familiar with TypeScript type annotations and basic type safety concepts.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            prompt += f"""
Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {student_response}

Include type annotations for all parameters and the return type.
Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a typescript code block:

```typescript
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
typescript code block. It is very important that you use the provided function name
when generating the code and include appropriate type annotations. For example:

```typescript
function {function_name}({params}): returnType {{
    // implementation here
}}
```

You may also use arrow function syntax with types if appropriate:

```typescript
const {function_name} = ({params}): returnType => {{
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
Always include proper TypeScript type annotations.
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract TypeScript code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```typescript\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            return [match.strip() for match in matches]

        # Method 2: If no markdown blocks, split by ```typescript
        if "```typescript" in llm_response:
            functions = list(
                map(
                    lambda x: x.split("```")[0].strip(),
                    llm_response.split("```typescript"),
                )
            )[1:]
            return functions

        # Method 3: Look for function definitions (both regular and arrow functions)
        # Regular function pattern with TypeScript types
        func_pattern = r"(function\s+\w+\s*(?:<[^>]+>)?\s*\([^)]*\)\s*:\s*\w+(?:<[^>]+>)?\s*{[^}]*})"
        # Arrow function pattern with TypeScript types
        arrow_pattern = r"((?:const|let|var)\s+\w+\s*(?::\s*[^=]+)?\s*=\s*(?:<[^>]+>)?\s*(?:\([^)]*\)|[^=>\s]+)\s*:\s*\w+(?:<[^>]+>)?\s*=>\s*(?:{[^}]*}|[^;]+);?)"
        # Async function pattern with TypeScript types
        async_pattern = r"(async\s+function\s+\w+\s*(?:<[^>]+>)?\s*\([^)]*\)\s*:\s*(?:Promise<[^>]+>|\w+)\s*{[^}]*})"
        # Generic function patterns (without explicit return type)
        generic_func_pattern = r"(function\s+\w+\s*(?:<[^>]+>)?\s*\([^)]*\)\s*{[^}]*})"
        generic_arrow_pattern = r"((?:const|let|var)\s+\w+\s*=\s*(?:<[^>]+>)?\s*(?:\([^)]*\)|[^=>\s]+)\s*=>\s*(?:{[^}]*}|[^;]+);?)"

        all_patterns = f"({func_pattern}|{arrow_pattern}|{async_pattern}|{generic_func_pattern}|{generic_arrow_pattern})"
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
        """Validate TypeScript syntax using tsc."""
        import subprocess
        import tempfile
        import os

        # Write code to a temporary file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".ts", delete=False) as f:
            f.write(code)
            temp_path = f.name

        try:
            # Use tsc --noEmit to validate syntax without generating output
            result = subprocess.run(
                ["tsc", "--noEmit", "--allowJs", "--checkJs", temp_path],
                capture_output=True,
                text=True,
            )

            if result.returncode == 0:
                return True, None
            else:
                # Extract error message
                error_msg = (
                    result.stdout.strip() if result.stdout else result.stderr.strip()
                )
                return False, f"TypeScript error: {error_msg}"

        except FileNotFoundError:
            # If tsc is not installed, try ts-node instead
            try:
                result = subprocess.run(
                    ["ts-node", "--transpile-only", "-e", f"import '{temp_path}'"],
                    capture_output=True,
                    text=True,
                )
                if result.returncode == 0:
                    return True, None
                else:
                    error_msg = result.stderr.strip()
                    return False, f"TypeScript error: {error_msg}"
            except FileNotFoundError:
                return False, "TypeScript compiler (tsc) or ts-node not found"
        except Exception as e:
            return False, f"Validation error: {str(e)}"
        finally:
            # Clean up temp file
            if os.path.exists(temp_path):
                os.remove(temp_path)
