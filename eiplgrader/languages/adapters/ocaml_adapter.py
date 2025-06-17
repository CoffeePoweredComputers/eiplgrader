"""OCaml language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides, SyntaxConventions


class OcamlAdapter(UnifiedLanguageAdapter):
    """OCaml language adapter - configuration driven"""

    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="ocaml",
            display_name="OCaml",
            file_extensions=[".ml"],
            compile_command=["ocamlc"],
            run_command=["./a.out"],
            
            # Enhanced specification
            code_block_tag="ocaml",
            student_model_template="""Pretend you are an introductory CS student learning OCaml for the very first
time. You have a rudimentary understanding of functions, pattern matching, let bindings, 
recursion, and basic types. You understand functional programming concepts but are still
learning the syntax and idioms of OCaml.""",
            
            # Syntax conventions
            syntax_conventions=SyntaxConventions(
                comment_single="(* *)",  # OCaml uses (* *) for comments
                comment_multi_start="(*",
                comment_multi_end="*)",
                statement_terminator=";;",  # For top-level expressions
                indentation_type="spaces",
                indentation_size=2,
            ),
            
            # Function patterns
            function_patterns=FunctionPatterns(
                definition_regex=r"(let\s+(?:rec\s+)?(\w+)\s+.*?=.*?)(?=let\s+(?:rec\s+)?\w+|$)",
                name_capture_group=2,
                requires_return_type=False,  # OCaml infers types
                supports_overloading=False,
                supports_default_params=False,
                supports_varargs=False
            ),
            
            # Validation
            validation_strategy="compiler",
            validation_command=["ocamlc", "-c"],
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "cgbg_instructions": """Include only the function and no additional test cases, code, or comments.
Use standard OCaml conventions - functions are lowercase with underscores, 
and use pattern matching where appropriate.
Respond with the code for the function {function_name} in the following format
which has the code wrapped in markdown of an ocaml code block:

```ocaml
<code here>
```""",
                    "redef_instructions": """Generate the code only and generate it to be surrounded with markdown of an
ocaml code block. It is very important that you use the provided function name
when generating the code. Use pattern matching and recursion where appropriate.
For example:

```ocaml
let {function_name} {params} =
  (* implementation *)
```""",
                    "multiple_versions_note": """Each version should use different OCaml idioms or approaches
(e.g., pattern matching vs if-then-else, tail recursion vs regular recursion, etc.)"""
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
            return f"Generate an OCaml function {function_name} that {student_response}"
        else:
            return f"Generate an OCaml function named {function_name}"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract OCaml code blocks
        patterns = [
            r'```ocaml\n(.*?)\n```',
            r'```ml\n(.*?)\n```',
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
        # Pattern for OCaml let bindings (functions)
        pattern = r"(let\s+(?:rec\s+)?(\w+)\s+.*?=.*?)"
        lines = code.split('\n')
        
        for i, line in enumerate(lines):
            match = re.search(pattern, line)
            if match:
                func_name = match.group(2)
                func_dict = {
                    'name': func_name,
                    'signature': line.strip(),
                    'start_line': i + 1,
                    'code': match.group(0),
                }
                functions.append(func_dict)
        
        return functions

    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        # Basic OCaml syntax validation using ocamlc
        try:
            import subprocess
            import tempfile
            import os
            
            # Create temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.ml', delete=False) as tmp:
                tmp.write(code)
                tmp_path = tmp.name
            
            try:
                result = subprocess.run(
                    ['ocamlc', '-c', tmp_path],
                    capture_output=True,
                    text=True,
                    timeout=10
                )
                
                if result.returncode == 0:
                    return True, None
                else:
                    return False, result.stderr
            finally:
                os.unlink(tmp_path)
                # Clean up compiled files
                cmi_file = tmp_path.replace('.ml', '.cmi')
                cmo_file = tmp_path.replace('.ml', '.cmo')
                if os.path.exists(cmi_file):
                    os.unlink(cmi_file)
                if os.path.exists(cmo_file):
                    os.unlink(cmo_file)
                
        except Exception as e:
            return False, str(e)

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove OCaml comments
        code = re.sub(r'\(\*.*?\*\)', '', code, flags=re.DOTALL)
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        return code



