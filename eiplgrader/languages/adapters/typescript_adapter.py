"""TypeScript language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class TypescriptAdapter(UnifiedLanguageAdapter):
    """TypeScript language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="typescript",
            display_name="TypeScript",
            file_extensions=[".ts"],
            run_command=["ts-node"],
            compile_command=["tsc"],
            
            # Enhanced specification
            code_block_tag="typescript",
            student_model_template="""Pretend you are an introductory CS student learning TypeScript for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You are familiar with TypeScript type annotations and basic type safety concepts.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="//",
                comment_multi_start="/*",
                comment_multi_end="*/",
                statement_terminator=";",
                indentation_type="spaces",
                indentation_size=2,
            ),
            
            # Function patterns (handle multiple function declaration styles)
            function_patterns=FunctionPatterns(
                definition_regex=r"((function\s+\w+\s*(?:<[^>]+>)?\s*\([^)]*\)\s*(?::\s*\w+(?:<[^>]+>)?)?\s*{[^}]*})|((?:const|let|var)\s+\w+\s*(?::\s*[^=]+)?\s*=\s*(?:<[^>]+>)?\s*(?:\([^)]*\)|[^=>\s]+)\s*(?::\s*\w+(?:<[^>]+>)?)?\s*=>\s*(?:{[^}]*}|[^;]+);?)|(async\s+function\s+\w+\s*(?:<[^>]+>)?\s*\([^)]*\)\s*:\s*(?:Promise<[^>]+>|\w+)\s*{[^}]*}))",
                name_capture_group=1,
                requires_return_type=False,  # TypeScript infers types
                supports_overloading=True,
                supports_default_params=True,
                supports_varargs=True
            ),
            
            # Validation
            validation_strategy="compiler",
            validation_command=["tsc", "--noEmit", "--allowJs", "--checkJs"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """Include type annotations for all parameters and the return type.
Include only the function and no additional test cases, code, or comments.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of a typescript code block:

```typescript
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of a
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
```""",
                    "multiple_versions_note": """You can use either function declaration or arrow function syntax.
Always include proper TypeScript type annotations."""
                }
            )
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
            return f"Generate a TypeScript function {function_name} that {student_response}"
        else:
            return f"Generate a TypeScript function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract TypeScript code blocks
        patterns = [
            r'```typescript\n(.*?)\n```',
            r'```ts\n(.*?)\n```',
            r'```javascript\n(.*?)\n```',  # TypeScript is superset of JS
            r'```js\n(.*?)\n```',
            r'```\n(.*?)\n```'  # Generic code block
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
        # TypeScript function patterns
        patterns = [
            r'function\s+(\w+)\s*(?:<[^>]+>)?\s*\([^)]*\)\s*(?::\s*\w+(?:<[^>]+>)?)?\s*\{[^}]*\}',  # function declaration
            r'(?:const|let|var)\s+(\w+)\s*(?::\s*[^=]+)?\s*=\s*(?:<[^>]+>)?\s*\([^)]*\)\s*(?::\s*\w+(?:<[^>]+>)?)?\s*=>\s*\{[^}]*\}',  # arrow function with block
            r'(?:const|let|var)\s+(\w+)\s*(?::\s*[^=]+)?\s*=\s*(?:<[^>]+>)?\s*\([^)]*\)\s*(?::\s*\w+(?:<[^>]+>)?)?\s*=>\s*[^;]+;?',  # arrow function expression
            r'async\s+function\s+(\w+)\s*(?:<[^>]+>)?\s*\([^)]*\)\s*:\s*(?:Promise<[^>]+>|\w+)\s*\{[^}]*\}'  # async function
        ]
        
        lines = code.split('\n')
        for i, line in enumerate(lines):
            for pattern in patterns:
                match = re.search(pattern, line)
                if match:
                    func_name = match.group(1)
                    func_dict = {
                        'name': func_name,
                        'signature': line.strip(),
                        'start_line': i + 1,
                        'code': match.group(0),
                    }
                    functions.append(func_dict)
                    break
        
        return functions

    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        # Use TypeScript compiler for validation
        try:
            import subprocess
            import tempfile
            import os
            
            # Create a temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.ts', delete=False) as f:
                f.write(code)
                temp_file = f.name
            
            try:
                result = subprocess.run(
                    ['tsc', '--noEmit', '--allowJs', '--checkJs', temp_file],
                    capture_output=True,
                    text=True,
                    timeout=10
                )
                if result.returncode == 0:
                    return True, None
                else:
                    return False, result.stderr
            finally:
                os.unlink(temp_file)
        except Exception as e:
            # Fallback to basic syntax checks if tsc is not available
            try:
                # Basic bracket matching
                open_braces = code.count('{')
                close_braces = code.count('}')
                open_parens = code.count('(')
                close_parens = code.count(')')
                open_brackets = code.count('[')
                close_brackets = code.count(']')
                
                if (open_braces != close_braces or 
                    open_parens != close_parens or 
                    open_brackets != close_brackets):
                    return False, "Mismatched brackets/parentheses"
                
                return True, None
            except Exception as fallback_e:
                return False, str(fallback_e)

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove comments
        code = re.sub(r'//.*', '', code)  # Single line comments
        code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)  # Multi-line comments
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code



