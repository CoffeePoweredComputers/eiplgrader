# SQL Language Implementation

This document describes the SQL language support implementation in eiplgrader.

## Overview

The SQL implementation differs from other language implementations in eiplgrader because SQL is a query language, not a programming language. Instead of generating and testing functions, it generates and tests SQL queries against test databases.

## Components

### SQL Adapter (`sql_adapter.py`)

The SQL adapter handles:
- Generating prompts for LLMs to create SQL queries
- Extracting SQL queries from LLM responses
- Basic syntax validation using SQLite's EXPLAIN QUERY PLAN

Key features:
- Supports both CGBG (code generation based grading) and redef (query redefinition) modes
- Automatically removes trailing semicolons from queries
- Validates syntax without requiring actual tables to exist

### SQL Executor (`sql_executor.py`)

The SQL executor:
- Creates temporary SQLite databases for each test
- Sets up schema and test data from test cases
- Executes queries and compares results
- Handles SELECT, INSERT, UPDATE, and DELETE operations

Key features:
- Flexible test data formats (dict or list)
- Smart result unwrapping (single values vs. dicts vs. lists)
- Order-insensitive comparison for result sets
- Proper cleanup of temporary databases

## Test Case Format

SQL test cases have a specific format:

```python
test_case = {
    "schema": [
        "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)",
        "CREATE TABLE orders (id INTEGER, user_id INTEGER, total REAL)"
    ],
    "test_data": [
        {"table": "users", "data": {"id": 1, "name": "Alice", "age": 25}},
        {"table": "orders", "data": {"id": 1, "user_id": 1, "total": 100.0}}
    ],
    "expected": [
        {"name": "Alice", "total": 100.0}
    ]
}
```

### Schema
List of CREATE TABLE statements to set up the database structure.

### Test Data
Can be provided in two formats:
1. List of dicts with "table" and "data" keys
2. Dict mapping table names to lists of rows
3. Raw SQL INSERT statements

### Expected Results
Can be:
- Scalar value for single-value results
- Dict for single-row results
- List of dicts for multi-row results
- Special formats:
  - `{"count": N}` - expects N rows
  - `{"contains": "value"}` - expects result to contain value

## Usage Example

```python
from eiplgrader.languages import language_registry

# Get SQL adapter and executor
sql_adapter = language_registry.get_adapter("sql")
sql_executor = language_registry.get_executor("sql")

# Generate SQL query prompt
prompt = sql_adapter.generate_prompt(
    student_response="find all users older than 18",
    function_name="query",
    gen_type="cgbg"
)

# Extract query from LLM response
queries = sql_adapter.extract_code(llm_response)

# Execute query with test case
result = sql_executor.execute_test(queries[0], test_case)
```

## Supported SQL Features

- SELECT queries with:
  - WHERE clauses
  - JOINs (INNER, LEFT, RIGHT)
  - GROUP BY and HAVING
  - ORDER BY
  - Aggregation functions (COUNT, SUM, AVG, MAX, MIN)
  - Subqueries
- INSERT statements
- UPDATE statements  
- DELETE statements
- CREATE TABLE (in schema setup)

## Implementation Notes

1. **SQLite Backend**: Uses SQLite for portability and simplicity
2. **No Semicolons**: Queries should not end with semicolons (automatically removed)
3. **Result Comparison**: Order-insensitive for result sets, handles different numeric types
4. **Syntax Validation**: Uses EXPLAIN QUERY PLAN for syntax checking without execution
5. **Temporary Databases**: Each test runs in an isolated temporary database