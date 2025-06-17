"""SQL language executor for query testing."""

import sqlite3
import tempfile
import os
import json
from typing import Dict, Any, List, Union
from ..base import LanguageExecutor


class SqlExecutor(LanguageExecutor):
    """Executor for SQL query testing using SQLite."""

    def __init__(self):
        self.temp_db = None
        self.connection = None

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare SQL code for execution.

        For SQL, this involves:
        1. Setting up the database schema if provided
        2. Inserting test data if provided
        3. Returning the query to execute
        """
        # The code is already the query, just ensure it's clean
        query = code.strip()
        if query.endswith(";"):
            query = query[:-1].strip()
        return query

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute SQL query with test case."""
        try:
            # Create temporary database
            temp_fd, self.temp_db = tempfile.mkstemp(suffix=".db")
            os.close(temp_fd)

            self.connection = sqlite3.connect(self.temp_db)
            cursor = self.connection.cursor()

            # Set up schema if provided
            schema = test_case.get("schema", [])
            if isinstance(schema, str):
                schema = [schema]

            for table_def in schema:
                if table_def.strip():
                    cursor.execute(table_def)

            # Insert test data if provided
            test_data = test_case.get("test_data", [])
            if isinstance(test_data, dict):
                # Convert dict format to list format
                test_data_list = []
                for table_name, rows in test_data.items():
                    for row in rows:
                        test_data_list.append({"table": table_name, "data": row})
                test_data = test_data_list

            for data_entry in test_data:
                if isinstance(data_entry, dict):
                    table = data_entry.get("table")
                    data = data_entry.get("data")
                    if table and data:
                        # Build INSERT statement
                        columns = list(data.keys())
                        values = list(data.values())
                        placeholders = ", ".join(["?" for _ in values])
                        col_names = ", ".join(columns)
                        insert_query = (
                            f"INSERT INTO {table} ({col_names}) VALUES ({placeholders})"
                        )
                        cursor.execute(insert_query, values)
                elif isinstance(data_entry, str):
                    # Raw SQL insert
                    cursor.execute(data_entry)

            self.connection.commit()

            # Prepare and execute the query
            query = self.prepare_code(code, test_case)
            cursor.execute(query)

            # Get results based on query type
            query_upper = query.upper().strip()

            if query_upper.startswith("SELECT"):
                # Fetch all results
                rows = cursor.fetchall()
                # Get column names
                columns = (
                    [desc[0] for desc in cursor.description]
                    if cursor.description
                    else []
                )

                # Convert to list of dicts for easier comparison
                if columns:
                    actual = [dict(zip(columns, row)) for row in rows]
                else:
                    actual = rows

                # Handle single row results
                if len(actual) == 1:
                    if isinstance(actual[0], dict):
                        # Check if we should unwrap based on expected format
                        expected = test_case.get("expected")
                        if isinstance(expected, dict):
                            # If expected is a dict, keep actual as dict
                            actual = actual[0]
                        elif len(actual[0]) == 1:
                            # Single value result, unwrap it
                            actual = list(actual[0].values())[0]
                        else:
                            # Multi-column result
                            actual = actual[0]
                    else:
                        actual = actual[0]
                elif len(actual) == 0:
                    actual = []

            elif query_upper.startswith(("INSERT", "UPDATE", "DELETE")):
                # For DML operations, return number of affected rows
                actual = cursor.rowcount
                self.connection.commit()
            else:
                # For other operations (CREATE, DROP, etc.)
                actual = "OK"
                self.connection.commit()

            # Compare with expected result
            expected = test_case.get("expected")

            # Handle different comparison types
            if isinstance(expected, dict) and "count" in expected:
                # Expecting a count
                passed = (
                    len(actual) == expected["count"]
                    if isinstance(actual, list)
                    else actual == expected["count"]
                )
            elif isinstance(expected, dict) and "contains" in expected:
                # Check if result contains certain values
                if isinstance(actual, list):
                    passed = any(expected["contains"] in str(item) for item in actual)
                else:
                    passed = expected["contains"] in str(actual)
            else:
                # Direct comparison
                passed = self._compare_results(actual, expected)

            return {
                "passed": passed,
                "actual": actual,
                "expected": expected,
                "query": query,
            }

        except sqlite3.Error as e:
            return {
                "passed": False,
                "error": f"SQL execution error: {str(e)}",
                "actual": None,
                "expected": test_case.get("expected"),
                "query": code,
            }
        except Exception as e:
            return {
                "passed": False,
                "error": f"Execution error: {str(e)}",
                "actual": None,
                "expected": test_case.get("expected"),
                "query": code,
            }
        finally:
            self.cleanup()

    def _compare_results(self, actual: Any, expected: Any) -> bool:
        """Compare actual and expected results with type flexibility."""
        # Handle None/NULL cases
        if actual is None and expected is None:
            return True
        if actual is None or expected is None:
            return False

        # Handle list comparisons (order-insensitive for SQL results)
        if isinstance(expected, list) and isinstance(actual, list):
            if len(expected) != len(actual):
                return False
            # For lists of dicts, compare without order
            if all(isinstance(item, dict) for item in expected):
                # Sort both lists by all keys for comparison
                try:
                    expected_sorted = sorted(
                        expected, key=lambda x: json.dumps(x, sort_keys=True)
                    )
                    actual_sorted = sorted(
                        actual, key=lambda x: json.dumps(x, sort_keys=True)
                    )
                    return expected_sorted == actual_sorted
                except:
                    # If sorting fails, do unordered comparison
                    for exp_item in expected:
                        if exp_item not in actual:
                            return False
                    return True
            else:
                # For simple lists, compare as sets
                return set(expected) == set(actual)

        # Handle dict comparisons
        if isinstance(expected, dict) and isinstance(actual, dict):
            if set(expected.keys()) != set(actual.keys()):
                return False
            for key in expected:
                if not self._compare_results(actual[key], expected[key]):
                    return False
            return True

        # Handle numeric comparisons (SQL might return different numeric types)
        if isinstance(expected, (int, float)) and isinstance(actual, (int, float)):
            return abs(expected - actual) < 1e-9

        # Direct comparison for other types
        return actual == expected

    def cleanup(self) -> None:
        """Clean up temporary database."""
        if self.connection:
            try:
                self.connection.close()
            except:
                pass
            self.connection = None

        if self.temp_db and os.path.exists(self.temp_db):
            try:
                os.remove(self.temp_db)
            except:
                pass
            self.temp_db = None
