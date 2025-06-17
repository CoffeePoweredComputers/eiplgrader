"""SQL language adapter for code generation."""

import re
from typing import List, Tuple, Optional
from ..base import LanguageAdapter, LanguageConfig


class SqlAdapter(LanguageAdapter):
    """Adapter for SQL language code generation."""

    def get_config(self) -> LanguageConfig:
        """Return SQL language configuration."""
        return LanguageConfig(
            name="sql",
            display_name="SQL",
            file_extensions=[".sql"],
            run_command=["sqlite3"],
        )

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate SQL-specific prompt for LLM."""

        # Base student model
        prompt = """Pretend you are an introductory CS student learning SQL for the very first
time. You have a rudimentary understanding of SELECT, INSERT, UPDATE, DELETE statements,
basic JOINs, WHERE clauses, and simple aggregations like COUNT, SUM, AVG.
"""

        if gen_type == "cgbg":
            # Code generation based grading
            # For SQL, function_name will be used as a hint for the query type
            prompt += f"""
Write a SQL query according to the following description:

{student_response}

Include only the SQL query and no additional explanations, comments, or code.
Respond with the SQL query in the following format wrapped in markdown of a sql code block:

```sql
<query here>
```

Do not include any semicolons at the end of the query.
"""
        elif gen_type == "redef":
            # Query redefinition based on schema
            schema = kwargs.get("schema", "")
            assumptions = kwargs.get("assumptions", "")

            prompt += f"""
Write a SQL query based on the following schema:
{schema}

You are given the following requirements:
{assumptions}

Generate only the SQL query and wrap it in markdown of a sql code block. 
Do not include any semicolons at the end of the query. For example:

```sql
SELECT * FROM table_name WHERE condition
```
"""

        # Add robustness prompt if generating multiple versions
        num_to_gen = kwargs.get("num_to_gen", 1)
        if num_to_gen > 1:
            prompt += f"""
Generate {num_to_gen} different versions of this query using different SQL approaches
(e.g., different JOIN types, subqueries vs JOINs, different aggregation methods).
"""

        return prompt

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract SQL code blocks from LLM response."""
        # Method 1: Extract markdown code blocks
        pattern = r"```sql\n(.*?)```"
        matches = re.findall(pattern, llm_response, re.DOTALL)

        if matches:
            # Clean up queries - remove semicolons and extra whitespace
            queries = []
            for match in matches:
                query = match.strip()
                # Remove trailing semicolon if present
                if query.endswith(";"):
                    query = query[:-1].strip()
                queries.append(query)
            return queries

        # Method 2: If no markdown blocks, look for SQL keywords
        sql_keywords = r"(?:SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER)"
        lines = llm_response.strip().split("\n")
        queries = []
        current_query = []

        for line in lines:
            if re.match(sql_keywords, line.strip(), re.IGNORECASE):
                if current_query:
                    query = "\n".join(current_query).strip()
                    if query.endswith(";"):
                        query = query[:-1].strip()
                    queries.append(query)
                current_query = [line]
            elif current_query:
                # Continue building the current query
                current_query.append(line)

        # Don't forget the last query
        if current_query:
            query = "\n".join(current_query).strip()
            if query.endswith(";"):
                query = query[:-1].strip()
            queries.append(query)

        # If still no queries found, return the entire response
        if not queries and llm_response.strip():
            query = llm_response.strip()
            if query.endswith(";"):
                query = query[:-1].strip()
            return [query]

        return queries

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate SQL syntax using sqlite3."""
        import sqlite3

        try:
            # Create an in-memory database for validation
            conn = sqlite3.connect(":memory:")
            cursor = conn.cursor()

            # Try to prepare the statement (this validates syntax)
            # Add a semicolon for validation if not present
            query = code.strip()
            if not query.endswith(";"):
                query += ";"

            # Use EXPLAIN QUERY PLAN to validate without executing
            cursor.execute(f"EXPLAIN QUERY PLAN {query}")

            conn.close()
            return True, None

        except sqlite3.Error as e:
            error_msg = str(e)
            # If the error is about missing tables, that's OK - syntax is still valid
            if "no such table" in error_msg or "no such column" in error_msg:
                return True, None
            return False, f"SQL syntax error: {error_msg}"
        except Exception as e:
            return False, f"Validation error: {str(e)}"
