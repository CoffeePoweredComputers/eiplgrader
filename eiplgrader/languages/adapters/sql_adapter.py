"""SQL language adapter using unified architecture."""

from typing import List, Optional, Tuple, Dict, Any
import re
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides


class SqlAdapter(UnifiedLanguageAdapter):
    """SQL language adapter - configuration driven"""
    
    def get_spec(self) -> LanguageSpec:
        return LanguageSpec(
            # Core configuration
            name="sql",
            display_name="SQL",
            file_extensions=[".sql"],
            run_command=["sqlite3"],
            
            # Enhanced specification
            code_block_tag="sql",
            student_model_template="You have a rudimentary understanding of SELECT, INSERT, UPDATE, DELETE statements, basic JOINs, WHERE clauses, and simple aggregations like COUNT, SUM, AVG.",
            
            # Function patterns (SQL queries)
            function_patterns=FunctionPatterns(
                definition_regex=r"((?:SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER).*?)(?=(?:SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER)|$)",
                name_capture_group=0
            ),
            
            # Validation
            validation_strategy="custom",
            
            # Template overrides
            template_overrides=TemplateOverrides(
                custom_templates={
                    "function_declaration_type": "SQL query",
                    "task_description": "Write a SQL query according to the following description:",
                    "function_example": "<query here>",
                    "language_specific_instructions": """Include only the SQL query and no additional explanations, comments, or code.
Respond with the SQL query in the following format wrapped in markdown of a sql code block:

```sql
<query here>
```

Do not include any semicolons at the end of the query.""",
                    "signature_preservation_note": """Generate only the SQL query and wrap it in markdown of a sql code block. 
Do not include any semicolons at the end of the query."""
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
            return f"Generate a SQL query that {student_response}"
        else:
            return f"Generate a SQL query"

    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        # Extract SQL code blocks
        patterns = [
            r'```sql\n(.*?)\n```',
            r'```SQL\n(.*?)\n```',
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
        # SQL doesn't have traditional functions, but we can extract queries
        functions = []
        # Match SQL statements
        patterns = [
            r'(SELECT\s+.*?)(?=SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|$)',
            r'(INSERT\s+.*?)(?=SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|$)',
            r'(UPDATE\s+.*?)(?=SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|$)',
            r'(DELETE\s+.*?)(?=SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|$)',
            r'(CREATE\s+.*?)(?=SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|$)',
            r'(DROP\s+.*?)(?=SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|$)',
            r'(ALTER\s+.*?)(?=SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|$)'
        ]
        
        lines = code.split('\n')
        for i, line in enumerate(lines):
            for j, pattern in enumerate(patterns):
                match = re.search(pattern, line, re.IGNORECASE | re.DOTALL)
                if match:
                    query_type = ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'CREATE', 'DROP', 'ALTER'][j]
                    func_dict = {
                        'name': f"{query_type.lower()}_query",
                        'signature': line.strip(),
                        'start_line': i + 1,
                        'code': match.group(1).strip(),
                    }
                    functions.append(func_dict)
                    break
        
        return functions

    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        # Basic SQL syntax validation
        try:
            # Simple validation - check for basic SQL keywords and structure
            sql_keywords = ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'CREATE', 'DROP', 'ALTER']
            code_upper = code.upper()
            
            # Check if it contains at least one SQL keyword
            has_sql_keyword = any(keyword in code_upper for keyword in sql_keywords)
            if not has_sql_keyword:
                return False, "No SQL keywords found"
            
            # Basic parentheses matching
            open_parens = code.count('(')
            close_parens = code.count(')')
            if open_parens != close_parens:
                return False, "Mismatched parentheses"
            
            return True, None
        except Exception as e:
            return False, str(e)

    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        # Remove SQL comments
        code = re.sub(r'--.*', '', code)  # Single line comments
        code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)  # Multi-line comments
        # Remove extra whitespace and normalize case
        code = re.sub(r'\s+', ' ', code)
        code = code.strip().upper()  # SQL is case-insensitive, normalize to upper
        return code