"""SQL language adapter using unified architecture."""

from typing import List, Optional
from ..base import UnifiedLanguageAdapter
from ..spec import LanguageSpec, FunctionPatterns, TemplateOverrides


class SqlAdapter(UnifiedLanguageAdapter):
    """SQL language adapter - configuration driven"""
    
    def get_language_spec(self) -> LanguageSpec:
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