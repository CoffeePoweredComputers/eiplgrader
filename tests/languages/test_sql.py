"""Test suite for SQL language adapter and executor."""
import pytest
from eiplgrader.languages.adapters.sql_adapter import SqlAdapter
from eiplgrader.languages.executors.sql_executor import SqlExecutor


class TestSqlAdapter:
    """Test the SQL language adapter."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = SqlAdapter()
    
    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "sql"
        assert config.display_name == "SQL"
        assert ".sql" in config.file_extensions
        assert config.run_command == ["sqlite3"]
    
    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="select all users who are older than 18",
            function_name="get_adult_users",
            gen_type="cgbg"
        )
        
        assert "select all users who are older than 18" in prompt
        assert "```sql" in prompt
        assert "SELECT" in prompt
        assert "semicolon" in prompt.lower()
    
    def test_generate_prompt_redef(self):
        """Test redef prompt generation with schema."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="query",
            gen_type="redef",
            schema="CREATE TABLE users (id INTEGER, name TEXT, age INTEGER)",
            assumptions="Get all users with age greater than 21"
        )
        
        assert "CREATE TABLE users" in prompt
        assert "Get all users with age greater than 21" in prompt
        assert "```sql" in prompt
    
    def test_generate_prompt_multiple(self):
        """Test prompt generation for multiple versions."""
        prompt = self.adapter.generate_prompt(
            student_response="join users and orders tables",
            function_name="join_query",
            gen_type="cgbg",
            num_to_gen=3
        )
        
        assert "3 different versions" in prompt
        assert "different SQL approaches" in prompt
    
    def test_extract_code_markdown(self):
        """Test extracting code from markdown blocks."""
        response = """Here's the query:
```sql
SELECT name, age FROM users WHERE age > 18
```
"""
        queries = self.adapter.extract_code(response)
        assert len(queries) == 1
        assert queries[0] == "SELECT name, age FROM users WHERE age > 18"
        assert not queries[0].endswith(';')  # Semicolon should be removed
    
    def test_extract_code_with_semicolon(self):
        """Test that semicolons are removed from queries."""
        response = """```sql
SELECT * FROM users;
```"""
        queries = self.adapter.extract_code(response)
        assert len(queries) == 1
        assert queries[0] == "SELECT * FROM users"
    
    def test_extract_code_multiple(self):
        """Test extracting multiple code blocks."""
        response = """Version 1:
```sql
SELECT * FROM users WHERE age > 18
```

Version 2:
```sql
SELECT id, name, age 
FROM users 
WHERE age > 18 
ORDER BY age DESC
```
"""
        queries = self.adapter.extract_code(response)
        assert len(queries) == 2
        assert "SELECT *" in queries[0]
        assert "ORDER BY age DESC" in queries[1]
    
    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown."""
        response = """SELECT name, age FROM users WHERE age > 18"""
        queries = self.adapter.extract_code(response)
        assert len(queries) == 1
        assert queries[0] == "SELECT name, age FROM users WHERE age > 18"
    
    def test_extract_code_multiple_statements(self):
        """Test extracting multiple SQL statements without markdown."""
        response = """SELECT * FROM users
INSERT INTO users (name, age) VALUES ('John', 25)
UPDATE users SET age = 26 WHERE name = 'John'"""
        queries = self.adapter.extract_code(response)
        assert len(queries) == 3
        assert queries[0].startswith("SELECT")
        assert queries[1].startswith("INSERT")
        assert queries[2].startswith("UPDATE")
    
    def test_validate_syntax_valid(self):
        """Test syntax validation with valid SQL."""
        code = "SELECT name, age FROM users WHERE age > 18"
        is_valid, error = self.adapter.validate_syntax(code)
        assert is_valid
        assert error is None
    
    def test_validate_syntax_valid_missing_table(self):
        """Test that missing tables don't fail syntax validation."""
        code = "SELECT * FROM non_existent_table"
        is_valid, error = self.adapter.validate_syntax(code)
        assert is_valid  # Syntax is valid even if table doesn't exist
    
    def test_validate_syntax_invalid(self):
        """Test syntax validation with invalid SQL."""
        code = "SELECT FROM WHERE"  # Invalid syntax
        is_valid, error = self.adapter.validate_syntax(code)
        assert not is_valid
        assert "SQL syntax error" in error


class TestSqlExecutor:
    """Test the SQL language executor."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.executor = SqlExecutor()
    
    def teardown_method(self):
        """Clean up after tests."""
        self.executor.cleanup()
    
    def test_simple_select(self):
        """Test executing a simple SELECT query."""
        code = "SELECT name, age FROM users WHERE age > 18"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [
                {"table": "users", "data": {"id": 1, "name": "Alice", "age": 25}},
                {"table": "users", "data": {"id": 2, "name": "Bob", "age": 17}},
                {"table": "users", "data": {"id": 3, "name": "Charlie", "age": 30}}
            ],
            "expected": [
                {"name": "Alice", "age": 25},
                {"name": "Charlie", "age": 30}
            ]
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert len(result["actual"]) == 2
    
    def test_select_with_aggregation(self):
        """Test SELECT with aggregation functions."""
        code = "SELECT COUNT(*) as total FROM users WHERE age >= 18"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [
                {"table": "users", "data": {"id": 1, "name": "Alice", "age": 25}},
                {"table": "users", "data": {"id": 2, "name": "Bob", "age": 17}},
                {"table": "users", "data": {"id": 3, "name": "Charlie", "age": 18}}
            ],
            "expected": {"total": 2}
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        # Since it's a single row with a named column, it should keep the dict format
        assert result["actual"] == {"total": 2}
    
    def test_select_single_value(self):
        """Test SELECT that returns a single value."""
        code = "SELECT MAX(age) FROM users"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [
                {"table": "users", "data": {"id": 1, "name": "Alice", "age": 25}},
                {"table": "users", "data": {"id": 2, "name": "Bob", "age": 17}},
                {"table": "users", "data": {"id": 3, "name": "Charlie", "age": 30}}
            ],
            "expected": 30
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 30
    
    def test_insert_operation(self):
        """Test INSERT operation."""
        code = "INSERT INTO users (name, age) VALUES ('David', 22)"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age INTEGER)"
            ],
            "test_data": [],
            "expected": 1  # One row affected
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 1
    
    def test_update_operation(self):
        """Test UPDATE operation."""
        code = "UPDATE users SET age = age + 1 WHERE age < 18"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [
                {"table": "users", "data": {"id": 1, "name": "Alice", "age": 25}},
                {"table": "users", "data": {"id": 2, "name": "Bob", "age": 17}},
                {"table": "users", "data": {"id": 3, "name": "Charlie", "age": 16}}
            ],
            "expected": 2  # Two rows affected
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 2
    
    def test_delete_operation(self):
        """Test DELETE operation."""
        code = "DELETE FROM users WHERE age < 18"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [
                {"table": "users", "data": {"id": 1, "name": "Alice", "age": 25}},
                {"table": "users", "data": {"id": 2, "name": "Bob", "age": 17}},
                {"table": "users", "data": {"id": 3, "name": "Charlie", "age": 16}}
            ],
            "expected": 2  # Two rows deleted
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 2
    
    def test_join_query(self):
        """Test JOIN operations."""
        code = """
        SELECT u.name, COUNT(o.id) as order_count
        FROM users u
        LEFT JOIN orders o ON u.id = o.user_id
        GROUP BY u.id, u.name
        ORDER BY u.name
        """
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)",
                "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER, total REAL)"
            ],
            "test_data": [
                {"table": "users", "data": {"id": 1, "name": "Alice"}},
                {"table": "users", "data": {"id": 2, "name": "Bob"}},
                {"table": "orders", "data": {"id": 1, "user_id": 1, "total": 100.0}},
                {"table": "orders", "data": {"id": 2, "user_id": 1, "total": 200.0}},
                {"table": "orders", "data": {"id": 3, "user_id": 2, "total": 150.0}}
            ],
            "expected": [
                {"name": "Alice", "order_count": 2},
                {"name": "Bob", "order_count": 1}
            ]
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
    
    def test_empty_result(self):
        """Test query that returns empty result set."""
        code = "SELECT * FROM users WHERE age > 100"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [
                {"table": "users", "data": {"id": 1, "name": "Alice", "age": 25}},
                {"table": "users", "data": {"id": 2, "name": "Bob", "age": 30}}
            ],
            "expected": []
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == []
    
    def test_syntax_error(self):
        """Test handling of SQL syntax errors."""
        code = "SELECT FROM WHERE"  # Invalid syntax
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [],
            "expected": []
        }
        
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        assert "error" in result
        assert "SQL execution error" in result["error"]
    
    def test_test_data_as_raw_sql(self):
        """Test using raw SQL for test data insertion."""
        code = "SELECT COUNT(*) FROM users"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [
                "INSERT INTO users VALUES (1, 'Alice', 25)",
                "INSERT INTO users VALUES (2, 'Bob', 30)"
            ],
            "expected": 2
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 2
    
    def test_test_data_as_dict_format(self):
        """Test using dict format for test data."""
        code = "SELECT name FROM users ORDER BY name"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)"
            ],
            "test_data": {
                "users": [
                    {"id": 1, "name": "Bob"},
                    {"id": 2, "name": "Alice"}
                ]
            },
            "expected": [
                {"name": "Alice"},
                {"name": "Bob"}
            ]
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
    
    def test_expected_count_format(self):
        """Test using count format for expected results."""
        code = "SELECT * FROM users WHERE age > 20"
        test_case = {
            "schema": [
                "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"
            ],
            "test_data": [
                {"table": "users", "data": {"id": 1, "name": "Alice", "age": 25}},
                {"table": "users", "data": {"id": 2, "name": "Bob", "age": 17}},
                {"table": "users", "data": {"id": 3, "name": "Charlie", "age": 30}}
            ],
            "expected": {"count": 2}
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
    
    def test_prepare_code_removes_semicolon(self):
        """Test that prepare_code removes trailing semicolons."""
        code = "SELECT * FROM users;"
        test_case = {}
        
        prepared = self.executor.prepare_code(code, test_case)
        assert prepared == "SELECT * FROM users"
        assert not prepared.endswith(';')
    
    def test_cleanup(self):
        """Test that cleanup removes temporary database."""
        # Execute a test to create temp database
        code = "SELECT 1"
        test_case = {"expected": 1}
        self.executor.execute_test(code, test_case)
        
        # Cleanup should remove the temp database
        # (cleanup is called automatically in execute_test)
        assert self.executor.temp_db is None
        assert self.executor.connection is None