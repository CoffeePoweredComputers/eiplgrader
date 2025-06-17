"""Unified prompt generation system for language adapters.

This module provides a centralized system for generating prompts across different
programming languages while maintaining language-specific nuances and conventions.
"""

import os
from typing import Dict, Any, Optional
from pathlib import Path

from .spec import LanguageSpec


class PromptValidationError(Exception):
    """Exception raised when prompt template validation fails."""
    pass


class PromptGenerator:
    """Unified prompt generation system for all language adapters.
    
    This class eliminates duplication in prompt generation by providing
    a template-based system that can be customized for each language
    while maintaining consistency across the codebase.
    """
    
    # Base templates for different generation types
    BASE_TEMPLATES = {
        "cgbg": "base_cgbg.txt",
        "redef": "base_redef.txt",
    }
    
    def __init__(self, template_dir: Optional[Path] = None):
        """Initialize the prompt generator.
        
        Args:
            template_dir: Custom template directory. Defaults to built-in templates.
        """
        if template_dir is None:
            self.template_dir = Path(__file__).parent / "templates"
        else:
            self.template_dir = Path(template_dir)
        
        self._template_cache: Dict[str, str] = {}
    
    def generate_prompt(
        self,
        language_spec: LanguageSpec,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs
    ) -> str:
        """Generate a prompt for the specified language and generation type.
        
        Args:
            language_spec: Language specification containing all language details
            student_response: The student's natural language response
            function_name: Name of the function to generate
            gen_type: Generation type ("cgbg" or "redef")
            **kwargs: Additional parameters (params, assumptions, num_to_gen, etc.)
            
        Returns:
            Generated prompt string ready for LLM consumption
            
        Raises:
            ValueError: If unsupported generation type is requested
            FileNotFoundError: If template file is not found
        """
        if gen_type not in self.BASE_TEMPLATES:
            raise ValueError(f"Unsupported generation type: {gen_type}")
        
        # Load base template
        template = self._load_template(gen_type, language_spec)
        
        # Build context for template substitution
        context = self._build_context(
            language_spec, student_response, function_name, gen_type, **kwargs
        )
        
        # Apply language-specific overrides
        context = self._apply_overrides(language_spec, context, gen_type)
        
        # Substitute template variables
        try:
            prompt = template.format(**context)
        except KeyError as e:
            raise ValueError(f"Missing template variable: {e}")
        
        return prompt
    
    def _load_template(self, gen_type: str, language_spec: LanguageSpec) -> str:
        """Load template with language-specific overrides.
        
        Args:
            gen_type: Generation type
            language_spec: Language specification
            
        Returns:
            Template string
        """
        # Check for language-specific template override
        override_template = getattr(language_spec.template_overrides, f"{gen_type}_template", None)
        if override_template:
            return override_template
        
        # Use base template
        template_file = self.BASE_TEMPLATES[gen_type]
        cache_key = f"{gen_type}_{template_file}"
        
        if cache_key not in self._template_cache:
            template_path = self.template_dir / template_file
            if not template_path.exists():
                raise FileNotFoundError(f"Template file not found: {template_path}")
            
            with open(template_path, 'r', encoding='utf-8') as f:
                self._template_cache[cache_key] = f.read()
        
        return self._template_cache[cache_key]
    
    def _build_context(
        self,
        language_spec: LanguageSpec,
        student_response: str,
        function_name: str,
        gen_type: str,
        **kwargs
    ) -> Dict[str, Any]:
        """Build context dictionary for template substitution.
        
        Args:
            language_spec: Language specification
            student_response: Student's response
            function_name: Function name
            gen_type: Generation type
            **kwargs: Additional parameters
            
        Returns:
            Context dictionary for template substitution
        """
        context = {
            # Basic information
            "language_display_name": language_spec.display_name,
            "language_name": language_spec.name,
            "student_response": student_response,
            "function_name": function_name,
            "code_block_tag": language_spec.get_code_block_tag(),
            
            # Student model description
            "student_model_description": self._get_student_model_description(language_spec),
            
            # Function declaration specifics
            "function_declaration_type": self._get_function_declaration_type(language_spec),
            
            # Multi-version generation
            "multi_version_instruction": self._get_multi_version_instruction(kwargs.get("num_to_gen", 1)),
        }
        
        # Add generation-type specific context
        if gen_type == "cgbg":
            context.update(self._build_cgbg_context(language_spec, function_name, **kwargs))
        elif gen_type == "redef":
            context.update(self._build_redef_context(language_spec, function_name, **kwargs))
        
        return context
    
    def _build_cgbg_context(
        self, language_spec: LanguageSpec, function_name: str, **kwargs
    ) -> Dict[str, Any]:
        """Build context specific to CGBG (Code Generation Based Grading).
        
        Args:
            language_spec: Language specification
            function_name: Function name
            **kwargs: Additional parameters
            
        Returns:
            CGBG-specific context
        """
        return {
            "language_specific_instructions": self._get_cgbg_instructions(language_spec),
            "function_example": self._get_cgbg_function_example(language_spec, function_name),
            "additional_conventions": self._get_cgbg_conventions(language_spec),
        }
    
    def _build_redef_context(
        self, language_spec: LanguageSpec, function_name: str, **kwargs
    ) -> Dict[str, Any]:
        """Build context specific to function redefinition.
        
        Args:
            language_spec: Language specification
            function_name: Function name
            **kwargs: Additional parameters
            
        Returns:
            Function redefinition-specific context
        """
        params = kwargs.get("params", "")
        assumptions = kwargs.get("assumptions", "")
        
        return {
            "function_signature_label": self._get_signature_label(language_spec),
            "function_signature": self._format_function_signature(language_spec, function_name, params),
            "assumptions": assumptions,
            "signature_preservation_note": self._get_signature_preservation_note(language_spec),
            "function_example": self._get_redef_function_example(language_spec, function_name, params),
            "language_specific_conventions": self._get_redef_conventions(language_spec),
        }
    
    def _apply_overrides(
        self, language_spec: LanguageSpec, context: Dict[str, Any], gen_type: str
    ) -> Dict[str, Any]:
        """Apply language-specific overrides to the context.
        
        Args:
            language_spec: Language specification
            context: Base context dictionary
            gen_type: Generation type
            
        Returns:
            Context with overrides applied
        """
        # Custom templates can override any context value
        custom_templates = language_spec.template_overrides.custom_templates
        if custom_templates:
            for key, template in custom_templates.items():
                if key in context:
                    try:
                        context[key] = template.format(**context)
                    except (KeyError, ValueError):
                        # If template substitution fails, keep original value
                        pass
        
        return context
    
    def _get_student_model_description(self, language_spec: LanguageSpec) -> str:
        """Get language-specific student model description.
        
        Args:
            language_spec: Language specification
            
        Returns:
            Student model description string
        """
        if language_spec.student_model_template:
            return language_spec.student_model_template
        
        # Default descriptions based on language characteristics
        base_concepts = "You have a rudimentary understanding of functions, loops, variables, and conditionals."
        
        language_specific = {
            "python": f"{base_concepts} You also don't know about type annotations.",
            "java": f"{base_concepts} You understand basic Java syntax including classes, static methods, and types.",
            "javascript": f"{base_concepts} You are familiar with ES6+ syntax including arrow functions and async/await.",
            "c": f"{base_concepts} You understand basic C syntax including headers, pointers, and memory management patterns.",
            "cpp": f"{base_concepts} You understand basic C++ syntax including classes, objects, and STL containers.",
            "haskell": f"{base_concepts} You understand functional programming concepts like pure functions, immutability, and lazy evaluation.",
            "rust": f"{base_concepts} You understand basic Rust syntax including ownership, borrowing, lifetimes, and error handling with Result types.",
            "sql": "You have a rudimentary understanding of SELECT, INSERT, UPDATE, DELETE statements, basic JOINs, WHERE clauses, and simple aggregations like COUNT, SUM, AVG.",
            "go": f"{base_concepts} You understand Go syntax including goroutines, channels, and error handling patterns.",
            "typescript": f"{base_concepts} You understand TypeScript syntax including type annotations, interfaces, and generics.",
        }
        
        return language_specific.get(language_spec.name, base_concepts)
    
    def _get_function_declaration_type(self, language_spec: LanguageSpec) -> str:
        """Get the appropriate function declaration terminology.
        
        Args:
            language_spec: Language specification
            
        Returns:
            Function declaration type (e.g., "function", "method", "query")
        """
        type_mapping = {
            "sql": "query",
            "java": "static method",
            "javascript": "function",
            "typescript": "function", 
            "c": "function",
            "cpp": "function",
            "python": "function",
            "rust": "function",
            "go": "function",
            "haskell": "function",
        }
        
        return type_mapping.get(language_spec.name, "function")
    
    def _get_cgbg_instructions(self, language_spec: LanguageSpec) -> str:
        """Get language-specific instructions for CGBG.
        
        Args:
            language_spec: Language specification
            
        Returns:
            Language-specific instruction string
        """
        instructions = {
            "java": "Use idiomatic Java style with proper type declarations.",
            "c": "The function should follow C conventions.",
            "haskell": "The function should follow Haskell conventions with proper type signatures.",
            "rust": "The function should follow Rust conventions and handle errors using Result<T, E> when appropriate.",
        }
        
        return instructions.get(language_spec.name, "")
    
    def _get_cgbg_function_example(self, language_spec: LanguageSpec, function_name: str) -> str:
        """Get language-specific function example for CGBG.
        
        Args:
            language_spec: Language specification
            function_name: Function name
            
        Returns:
            Function example string
        """
        examples = {
            "python": "<code here>",
            "java": f"""public class Solution {{
    public static <return_type> {function_name}(<parameters>) {{
        // implementation
    }}
}}""",
            "javascript": "<code here>",
            "c": f"""#include <stdio.h>
#include <stdlib.h>

returnType {function_name}(parameters) {{
    // function body
}}""",
            "haskell": f"""{function_name} :: Type -> Type -> ReturnType
{function_name} param1 param2 = expression""",
            "rust": f"""fn {function_name}(parameters) -> ReturnType {{
    // function body
}}""",
            "sql": "<query here>",
        }
        
        return examples.get(language_spec.name, "<code here>")
    
    def _get_cgbg_conventions(self, language_spec: LanguageSpec) -> str:
        """Get additional conventions for CGBG.
        
        Args:
            language_spec: Language specification
            
        Returns:
            Additional conventions string
        """
        conventions = {
            "c": """
Include necessary headers like stdio.h and stdlib.h if needed.
Use proper C types (int, char, float, double, etc.).
If working with arrays, remember C arrays are passed as pointers.""",
            "haskell": """
Use proper Haskell types (Int, Integer, Double, String, [a], etc.).
Include the type signature for the function.
Use pattern matching where appropriate.
Remember that Haskell functions are pure and immutable.""",
            "rust": """
If the function can fail, use Rust's idiomatic error handling pattern with Result<T, E>.
Follow Rust ownership rules and borrowing conventions. Use references (&) when you don't
need to take ownership of parameters.""",
            "sql": """
Do not include any semicolons at the end of the query.""",
        }
        
        return conventions.get(language_spec.name, "")
    
    def _get_signature_label(self, language_spec: LanguageSpec) -> str:
        """Get appropriate signature label for the language.
        
        Args:
            language_spec: Language specification
            
        Returns:
            Signature label (e.g., "function signature", "method signature")
        """
        labels = {
            "java": "method signature",
            "sql": "schema",
        }
        
        return labels.get(language_spec.name, "function name")
    
    def _format_function_signature(self, language_spec: LanguageSpec, function_name: str, params: str) -> str:
        """Format function signature according to language conventions.
        
        Args:
            language_spec: Language specification
            function_name: Function name
            params: Parameters string
            
        Returns:
            Formatted function signature
        """
        formats = {
            "python": f"def {function_name}({params}):",
            "java": f"public static <return_type> {function_name}({params})",
            "javascript": f"function {function_name}({params}) {{",
            "c": f"{function_name}({params})",
            "haskell": f"{function_name}",
            "rust": f"fn {function_name}({params}) -> ReturnType",
            "sql": params,  # For SQL, params contains the schema
        }
        
        return formats.get(language_spec.name, f"{function_name}({params})")
    
    def _get_signature_preservation_note(self, language_spec: LanguageSpec) -> str:
        """Get note about preserving function signature.
        
        Args:
            language_spec: Language specification
            
        Returns:
            Signature preservation note
        """
        notes = {
            "java": " and include the type signature",
            "haskell": " and include the type signature",
        }
        
        return notes.get(language_spec.name, "")
    
    def _get_redef_function_example(self, language_spec: LanguageSpec, function_name: str, params: str) -> str:
        """Get function example for redefinition.
        
        Args:
            language_spec: Language specification
            function_name: Function name
            params: Parameters
            
        Returns:
            Function example for redefinition
        """
        examples = {
            "python": f"""def {function_name}({params}):
    pass""",
            "java": f"""public class Solution {{
    public static <return_type> {function_name}({params}) {{
        // implementation
    }}
}}""",
            "javascript": f"""function {function_name}({params}) {{
    // implementation here
}}""",
            "c": f"""#include <stdio.h>
#include <stdlib.h>

returnType {function_name}({params}) {{
    // function implementation
}}""",
            "haskell": f"""{function_name} :: Type -> Type -> ReturnType
{function_name} {params} = implementation""",
            "rust": f"""fn {function_name}({params}) -> ReturnType {{
    // function implementation
}}""",
            "sql": "SELECT * FROM table_name WHERE condition",
        }
        
        return examples.get(language_spec.name, f"{function_name}({params})")
    
    def _get_redef_conventions(self, language_spec: LanguageSpec) -> str:
        """Get language-specific conventions for redefinition.
        
        Args:
            language_spec: Language specification
            
        Returns:
            Conventions string
        """
        conventions = {
            "java": """
The method must be static and placed within a Solution class.""",
            "javascript": """
You may also use arrow function syntax if appropriate:

```javascript
const {function_name} = ({params}) => {{
    // implementation here
}};
```""",
            "c": """
Use standard C conventions for parameter passing:
- Pass by value for basic types
- Pass pointers for arrays or when modifications are needed
- Use const for read-only pointer parameters""",
            "haskell": """
Use standard Haskell conventions:
- Include type signatures
- Use pattern matching for different cases
- Keep functions pure (no side effects)
- Use recursion instead of loops
- Use guards or if-then-else for conditionals""",
            "rust": """
If the function should handle errors, use Result<T, E> as the return type
following Rust conventions. Follow ownership and borrowing rules - use references
when appropriate and avoid unnecessary clones.""",
        }
        
        return conventions.get(language_spec.name, "")
    
    def _get_multi_version_instruction(self, num_to_gen: int) -> str:
        """Get multi-version generation instruction.
        
        Args:
            num_to_gen: Number of versions to generate
            
        Returns:
            Multi-version instruction string or empty string
        """
        if num_to_gen <= 1:
            return ""
        
        return f"""
Generate {num_to_gen} different versions of this function with these formatting
constraints."""
    
    def validate_template(self, template_path: Path) -> bool:
        """Validate a template file for correctness.
        
        Args:
            template_path: Path to template file
            
        Returns:
            True if template is valid
            
        Raises:
            PromptValidationError: If template has issues
        """
        if not template_path.exists():
            raise PromptValidationError(f"Template file not found: {template_path}")
        
        try:
            with open(template_path, 'r', encoding='utf-8') as f:
                template_content = f.read()
        except Exception as e:
            raise PromptValidationError(f"Cannot read template file: {e}")
        
        # Check for required template variables (all templates need these core ones)
        required_vars = {
            "language_display_name", "code_block_tag"
        }
        
        missing_vars = []
        for var in required_vars:
            if f"{{{var}}}" not in template_content:
                missing_vars.append(var)
        
        if missing_vars:
            raise PromptValidationError(
                f"Template missing required variables: {missing_vars}"
            )
        
        # Check for balanced braces
        open_braces = template_content.count("{")
        close_braces = template_content.count("}")
        
        if open_braces != close_braces:
            raise PromptValidationError(
                f"Unbalanced template braces: {open_braces} open, {close_braces} close"
            )
        
        # Test template formatting with dummy data
        try:
            dummy_context = {
                "language_display_name": "TestLang",
                "language_name": "testlang", 
                "student_response": "test response",
                "function_name": "testFunction",
                "code_block_tag": "testlang",
                "student_model_description": "test description",
                "function_declaration_type": "function",
                "multi_version_instruction": "",
                "language_specific_instructions": "",
                "function_example": "test example",
                "additional_conventions": "",
                "function_signature_label": "function signature",
                "function_signature": "testFunction()",
                "assumptions": "test assumptions",
                "signature_preservation_note": "",
                "language_specific_conventions": "",
            }
            
            template_content.format(**dummy_context)
            
        except KeyError as e:
            raise PromptValidationError(
                f"Template contains undefined variable: {e}"
            )
        except Exception as e:
            raise PromptValidationError(
                f"Template formatting error: {e}"
            )
        
        return True
    
    def validate_all_templates(self) -> Dict[str, bool]:
        """Validate all templates in the template directory.
        
        Returns:
            Dictionary mapping template names to validation status
        """
        results = {}
        
        for template_file in self.BASE_TEMPLATES.values():
            template_path = self.template_dir / template_file
            try:
                results[template_file] = self.validate_template(template_path)
            except PromptValidationError as e:
                results[template_file] = False
                print(f"Validation failed for {template_file}: {e}")
        
        return results