"""Integration tests for OCaml language support."""

import pytest
import tempfile
import os
from unittest.mock import patch, Mock

from eiplgrader.languages import language_registry
from eiplgrader.codegen import CodeGenerator


class TestOcamlIntegration:
    """Test OCaml integration with the full system."""

    def test_language_registered(self):
        """Test that OCaml is properly registered."""
        languages = language_registry.list_languages()
        assert "ocaml" in languages

        adapter = language_registry.get_adapter("ocaml")
        executor = language_registry.get_executor("ocaml")

        assert adapter is not None
        assert executor is not None

        # Check config
        config = adapter.get_config()
        assert config.name == "ocaml"
        assert config.display_name == "OCaml"

    @patch("subprocess.run")
    def test_full_workflow(self, mock_run):
        """Test full workflow from prompt to execution."""
        # Mock successful compilation and execution
        compile_result = Mock(returncode=0, stderr="", stdout="")
        exec_result = Mock(returncode=0, stderr="", stdout="15\n")
        mock_run.side_effect = [compile_result, exec_result]

        # Get adapter and executor
        adapter = language_registry.get_adapter("ocaml")
        executor = language_registry.get_executor("ocaml")

        # Generate prompt
        prompt = adapter.generate_prompt(
            student_response="returns the sum of two integers",
            function_name="sum",
            gen_type="cgbg",
        )

        assert "OCaml" in prompt
        assert "sum" in prompt

        # Simulate LLM response
        llm_response = """
        Here's the OCaml function:
        
        ```ocaml
        let sum a b = a + b
        ```
        """

        # Extract code
        code_blocks = adapter.extract_code(llm_response)
        assert len(code_blocks) == 1
        assert "let sum a b = a + b" in code_blocks[0]

        # Execute test
        test_case = {
            "function_name": "sum",
            "parameters": {"a": 7, "b": 8},
            "expected": 15,
        }

        result = executor.execute_test(code_blocks[0], test_case)
        assert result["passed"] is True
        assert result["actual"] == 15
        assert result["expected"] == 15

        # Cleanup
        executor.cleanup()

    def test_ocaml_specific_features(self):
        """Test OCaml-specific language features."""
        adapter = language_registry.get_adapter("ocaml")

        # Test pattern matching extraction
        llm_response = """
        ```ocaml
        let rec factorial n =
          match n with
          | 0 -> 1
          | n -> n * factorial (n - 1)
        ```
        """

        code_blocks = adapter.extract_code(llm_response)
        assert len(code_blocks) == 1
        assert "match n with" in code_blocks[0]
        assert "| 0 -> 1" in code_blocks[0]

        # Test tail recursion extraction
        llm_response = """
        ```ocaml
        let sum_list lst =
          let rec aux lst acc =
            match lst with
            | [] -> acc
            | h::t -> aux t (acc + h)
          in
          aux lst 0
        ```
        """

        code_blocks = adapter.extract_code(llm_response)
        assert len(code_blocks) == 1
        assert "let rec aux" in code_blocks[0]
        assert "aux lst 0" in code_blocks[0]

    @patch("subprocess.run")
    def test_error_handling(self, mock_run):
        """Test error handling in OCaml execution."""
        # Test syntax error
        syntax_error_result = Mock(
            returncode=1, stderr='File "test.ml", line 1: Syntax error', stdout=""
        )
        mock_run.return_value = syntax_error_result

        adapter = language_registry.get_adapter("ocaml")
        valid, error = adapter.validate_syntax("let = bad syntax")

        assert valid is False
        assert "Syntax error" in error

        # Test runtime error
        executor = language_registry.get_executor("ocaml")

        compile_ok = Mock(returncode=0, stderr="", stdout="")
        runtime_error = Mock(returncode=1, stderr="Exception: Match_failure", stdout="")
        mock_run.side_effect = [compile_ok, runtime_error]

        code = "let head lst = match lst with | h::_ -> h"
        test_case = {
            "function_name": "head",
            "parameters": {"lst": []},  # Empty list will cause Match_failure
            "expected": 0,
        }

        result = executor.execute_test(code, test_case)
        assert result["passed"] is False
        assert "Match_failure" in result["error"]

        executor.cleanup()
