"""Tests for OCaml language adapter and executor."""

import pytest
from unittest.mock import Mock, patch
import json

from eiplgrader.languages.adapters.ocaml_adapter import OcamlAdapter
from eiplgrader.languages.executors.ocaml_executor import OcamlExecutor


class TestOcamlAdapter:
    """Test OCaml adapter functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = OcamlAdapter()

    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "ocaml"
        assert config.display_name == "OCaml"
        assert config.file_extensions == [".ml"]
        assert config.compile_command == ["ocamlc"]
        assert config.run_command == ["./a.out"]

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="calculates the factorial of a number",
            function_name="factorial",
            gen_type="cgbg",
        )

        assert "OCaml" in prompt
        assert "factorial" in prompt
        assert "calculates the factorial of a number" in prompt
        assert "```ocaml" in prompt
        assert "pattern matching" in prompt

    def test_generate_prompt_redef(self):
        """Test function redefinition prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="add",
            gen_type="redef",
            params="a b",
            assumptions="a and b are integers",
        )

        assert "let add a b =" in prompt
        assert "a and b are integers" in prompt
        assert "```ocaml" in prompt

    def test_generate_prompt_multiple_versions(self):
        """Test prompt with multiple versions requested."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers",
            function_name="add",
            gen_type="cgbg",
            num_to_gen=3,
        )

        assert "3 different versions" in prompt
        assert "different OCaml idioms" in prompt

    def test_extract_code_markdown_blocks(self):
        """Test extracting code from markdown blocks."""
        llm_response = """Here's the code:
        
```ocaml
let rec factorial n =
  match n with
  | 0 -> 1
  | n -> n * factorial (n - 1)
```

And another version:

```ocaml
let factorial n =
  let rec aux n acc =
    if n <= 0 then acc
    else aux (n - 1) (n * acc)
  in
  aux n 1
```
"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 2
        assert "let rec factorial n =" in code_blocks[0]
        assert "let factorial n =" in code_blocks[1]
        assert "aux" in code_blocks[1]

    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        llm_response = """let add a b = a + b

let rec map f lst =
  match lst with
  | [] -> []
  | h::t -> (f h) :: (map f t)"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 2
        assert "let add a b" in code_blocks[0]
        assert "let rec map f lst" in code_blocks[1]

    def test_extract_code_empty_response(self):
        """Test extracting code from empty response."""
        code_blocks = self.adapter.extract_code("")
        assert code_blocks == []

    @patch("subprocess.run")
    def test_validate_syntax_valid(self, mock_run):
        """Test syntax validation with valid code."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="")

        valid, error = self.adapter.validate_syntax("let add a b = a + b")
        assert valid is True
        assert error is None

    @patch("subprocess.run")
    def test_validate_syntax_invalid(self, mock_run):
        """Test syntax validation with invalid code."""
        mock_run.return_value = Mock(
            returncode=1,
            stderr='File "test.ml", line 1, characters 4-5:\nSyntax error',
            stdout="",
        )

        valid, error = self.adapter.validate_syntax("let = invalid")
        assert valid is False
        assert "Syntax error" in error

    @patch("subprocess.run")
    def test_validate_syntax_timeout(self, mock_run):
        """Test syntax validation timeout."""
        import subprocess

        mock_run.side_effect = subprocess.TimeoutExpired("ocamlc", 5)

        valid, error = self.adapter.validate_syntax(
            "let infinite = while true do () done"
        )
        assert valid is False
        assert "timeout" in error.lower()

    @patch("subprocess.run")
    def test_validate_syntax_no_compiler(self, mock_run):
        """Test syntax validation when ocamlc is not installed."""
        mock_run.side_effect = FileNotFoundError()

        valid, error = self.adapter.validate_syntax("let add a b = a + b")
        assert valid is True  # Skip validation if compiler not available
        assert error is None


class TestOcamlExecutor:
    """Test OCaml executor functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = OcamlExecutor()

    def test_prepare_code_simple_function(self):
        """Test preparing code for a simple function."""
        code = "let add a b = a + b"
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
        }

        prepared = self.executor.prepare_code(code, test_case)

        # Check for JSON parsing helpers
        assert "parse_json_object" in prepared
        assert "json_of_int" in prepared

        # Check for parameter extraction
        assert 'let a = int_of_string (get_json_field json_obj "a")' in prepared
        assert 'let b = int_of_string (get_json_field json_obj "b")' in prepared

        # Check for function call
        assert "let result = add a b" in prepared

        # Check for output
        assert "print_string (json_of_int result)" in prepared

    def test_prepare_code_list_parameters(self):
        """Test preparing code with list parameters."""
        code = "let sum_list lst = List.fold_left (+) 0 lst"
        test_case = {
            "function_name": "sum_list",
            "parameters": {"lst": [1, 2, 3, 4, 5]},
            "expected": 15,
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "parse_int_list" in prepared
        assert 'let lst = parse_int_list (get_json_field json_obj "lst")' in prepared

    def test_prepare_code_string_parameters(self):
        """Test preparing code with string parameters."""
        code = 'let greet name = "Hello, " ^ name ^ "!"'
        test_case = {
            "function_name": "greet",
            "parameters": {"name": "World"},
            "expected": "Hello, World!",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert 'let name = get_json_field json_obj "name"' in prepared
        assert "json_of_string result" in prepared

    def test_prepare_code_boolean_result(self):
        """Test preparing code that returns a boolean."""
        code = "let is_even n = n mod 2 = 0"
        test_case = {
            "function_name": "is_even",
            "parameters": {"n": 4},
            "expected": True,
        }

        prepared = self.executor.prepare_code(code, test_case)
        assert "json_of_bool result" in prepared

    def test_prepare_code_no_parameters(self):
        """Test preparing code with no parameters."""
        code = "let get_pi () = 3.14159"
        test_case = {"function_name": "get_pi", "parameters": {}, "expected": 3.14159}

        prepared = self.executor.prepare_code(code, test_case)
        assert "let result = get_pi ()" in prepared
        assert "json_of_float result" in prepared

    def test_prepare_code_inplace_mode_1(self):
        """Test preparing code for in-place modification (mode 1)."""
        code = "let increment r = r := !r + 1"
        test_case = {
            "function_name": "increment",
            "parameters": {"r": 5},
            "expected": 6,
            "inplace": "1",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "let r_val = int_of_string" in prepared
        assert "let r = ref r_val" in prepared
        assert "let () = increment r" in prepared
        assert "json_of_int !r" in prepared

    def test_prepare_code_inplace_mode_2(self):
        """Test preparing code for in-place modification with return (mode 2)."""
        code = "let increment_and_return r = r := !r + 1; !r"
        test_case = {
            "function_name": "increment_and_return",
            "parameters": {"r": 5},
            "expected": 6,
            "inplace": "2",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "let r = ref r_val" in prepared
        assert "let result = increment_and_return r" in prepared
        assert "json_of_int result" in prepared

    @patch("subprocess.run")
    def test_compile_success(self, mock_run):
        """Test successful compilation."""
        mock_run.return_value = Mock(returncode=0, stderr="", stdout="")

        success, output_path, error = self.executor.compile("/tmp/test.ml")

        assert success is True
        assert output_path == "/tmp/test"
        assert error == ""

        # Check compile command
        mock_run.assert_called_once()
        args = mock_run.call_args[0][0]
        assert args[0] == "ocamlc"
        assert "-o" in args
        assert "/tmp/test.ml" in args

    @patch("subprocess.run")
    def test_compile_failure(self, mock_run):
        """Test compilation failure."""
        mock_run.return_value = Mock(
            returncode=1, stderr="Error: Unbound value foo", stdout=""
        )

        success, output_path, error = self.executor.compile("/tmp/test.ml")

        assert success is False
        assert "Unbound value foo" in error

    @patch("os.path.exists")
    @patch("shutil.rmtree")
    @patch("os.walk")
    @patch("os.unlink")
    def test_cleanup(self, mock_unlink, mock_walk, mock_rmtree, mock_exists):
        """Test cleanup of temporary files."""
        mock_exists.return_value = True
        mock_walk.return_value = [
            ("/tmp/test", [], ["test.ml", "test.cmi", "test.cmo", "test"])
        ]

        self.executor.temp_dir = "/tmp/test"
        self.executor.cleanup()

        # Should remove .cmi and .cmo files
        assert mock_unlink.call_count == 2
        mock_unlink.assert_any_call("/tmp/test/test.cmi")
        mock_unlink.assert_any_call("/tmp/test/test.cmo")

        # Should remove temp directory
        mock_rmtree.assert_called_once_with("/tmp/test")

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_success(self, mock_mkdtemp, mock_run):
        """Test successful test execution."""
        mock_mkdtemp.return_value = "/tmp/test"

        # Mock compilation success
        compile_result = Mock(returncode=0, stderr="", stdout="")
        # Mock execution success
        exec_result = Mock(returncode=0, stderr="", stdout="8\n")

        mock_run.side_effect = [compile_result, exec_result]

        code = "let add a b = a + b"
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8
        assert "error" not in result or result.get("error") is None

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_wrong_output(self, mock_mkdtemp, mock_run):
        """Test test execution with wrong output."""
        mock_mkdtemp.return_value = "/tmp/test"

        compile_result = Mock(returncode=0, stderr="", stdout="")
        exec_result = Mock(returncode=0, stderr="", stdout="7\n")

        mock_run.side_effect = [compile_result, exec_result]

        code = "let add a b = a + b - 1"  # Wrong implementation
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is False
        assert result["actual"] == 7
        assert result["expected"] == 8

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_compilation_error(self, mock_mkdtemp, mock_run):
        """Test test execution with compilation error."""
        mock_mkdtemp.return_value = "/tmp/test"

        compile_result = Mock(returncode=1, stderr="Syntax error", stdout="")
        mock_run.return_value = compile_result

        code = "let add a = invalid syntax"
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is False
        assert "Compilation failed" in result["error"]
        assert "Syntax error" in result["error"]
        assert result["actual"] is None

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_runtime_error(self, mock_mkdtemp, mock_run):
        """Test test execution with runtime error."""
        mock_mkdtemp.return_value = "/tmp/test"

        compile_result = Mock(returncode=0, stderr="", stdout="")
        exec_result = Mock(
            returncode=1, stderr="Exception: Division_by_zero", stdout=""
        )

        mock_run.side_effect = [compile_result, exec_result]

        code = "let divide a b = a / b"
        test_case = {
            "function_name": "divide",
            "parameters": {"a": 5, "b": 0},
            "expected": 0,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is False
        assert "Runtime error" in result["error"]
        assert "Division_by_zero" in result["error"]
        assert result["actual"] is None
