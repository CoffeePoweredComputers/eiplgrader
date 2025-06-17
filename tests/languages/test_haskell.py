"""Tests for Haskell language adapter and executor."""

import pytest
from unittest.mock import patch, MagicMock
from eiplgrader.languages.adapters.haskell_adapter import HaskellAdapter
from eiplgrader.languages.executors.haskell_executor import HaskellExecutor


class TestHaskellAdapter:
    """Test cases for HaskellAdapter."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = HaskellAdapter()

    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "haskell"
        assert config.display_name == "Haskell"
        assert config.file_extensions == [".hs"]
        assert config.compile_command == ["ghc"]
        assert config.run_command is None

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg",
        )

        assert "add" in prompt
        assert "adds two numbers together" in prompt
        assert "```haskell" in prompt
        assert "Type -> Type -> ReturnType" in prompt
        assert "pattern matching" in prompt
        assert "pure" in prompt

    def test_generate_prompt_redef(self):
        """Test function redefinition prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="multiply",
            gen_type="redef",
            params="x y",
            assumptions="x and y are integers",
        )

        assert "multiply" in prompt
        assert "x y" in prompt
        assert "x and y are integers" in prompt
        assert "type signature" in prompt

    def test_generate_prompt_multiple_versions(self):
        """Test prompt generation for multiple versions."""
        prompt = self.adapter.generate_prompt(
            student_response="calculates factorial",
            function_name="factorial",
            gen_type="cgbg",
            num_to_gen=3,
        )

        assert "3 different versions" in prompt

    def test_extract_code_markdown_blocks(self):
        """Test extracting code from markdown blocks."""
        llm_response = """Here's the function:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

And another version:

```haskell
add :: Num a => a -> a -> a
add = (+)
```
"""

        codes = self.adapter.extract_code(llm_response)
        assert len(codes) == 2
        assert "add :: Int -> Int -> Int" in codes[0]
        assert "add :: Num a => a -> a -> a" in codes[1]

    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        llm_response = """factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)"""

        codes = self.adapter.extract_code(llm_response)
        assert len(codes) >= 1
        assert "factorial" in codes[0]

    def test_extract_code_simple_function(self):
        """Test extracting simple function without type signature."""
        llm_response = "double x = x * 2"

        codes = self.adapter.extract_code(llm_response)
        assert len(codes) == 1
        assert "double x = x * 2" in codes[0]

    @patch("subprocess.run")
    def test_validate_syntax_valid(self, mock_run):
        """Test syntax validation with valid code."""
        mock_run.return_value = MagicMock(returncode=0, stderr="")

        code = """add :: Int -> Int -> Int
add x y = x + y"""

        is_valid, error = self.adapter.validate_syntax(code)
        assert is_valid
        assert error is None

    @patch("subprocess.run")
    def test_validate_syntax_invalid(self, mock_run):
        """Test syntax validation with invalid code."""
        mock_run.return_value = MagicMock(
            returncode=1, stderr="test.hs:3:1: error: parse error on input 'add'"
        )

        code = """add :: Int -> Int -> Int
add x y = """

        is_valid, error = self.adapter.validate_syntax(code)
        assert not is_valid
        assert "Syntax error" in error

    @patch("subprocess.run")
    def test_validate_syntax_no_ghc(self, mock_run):
        """Test syntax validation when ghc is not available."""
        mock_run.side_effect = FileNotFoundError()

        code = "add x y = x + y"
        is_valid, error = self.adapter.validate_syntax(code)
        assert is_valid  # Should pass if ghc not available
        assert error is None


class TestHaskellExecutor:
    """Test cases for HaskellExecutor."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = HaskellExecutor()

    def test_prepare_code_simple_function(self):
        """Test preparing code for a simple function."""
        code = """add :: Int -> Int -> Int
add x y = x + y"""

        test_case = {
            "function_name": "add",
            "parameters": {"x": 5, "y": 3},
            "expected": 8,
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "module Main where" in prepared
        assert "import System.IO" in prepared
        assert "add :: Int -> Int -> Int" in prepared
        assert "main :: IO ()" in prepared
        assert 'parseParam "x"' in prepared
        assert 'parseParam "y"' in prepared
        assert "let result = add x y" in prepared

    def test_prepare_code_with_list(self):
        """Test preparing code with list parameters."""
        code = """sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs"""

        test_case = {
            "function_name": "sumList",
            "parameters": {"lst": [1, 2, 3, 4, 5]},
            "expected": 15,
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "parseIntList" in prepared
        assert "let result = sumList lst" in prepared

    def test_prepare_code_string_function(self):
        """Test preparing code with string parameters."""
        code = """reverseString :: String -> String
reverseString = reverse"""

        test_case = {
            "function_name": "reverseString",
            "parameters": {"s": "hello"},
            "expected": "olleh",
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert 'parseStringParam "s"' in prepared
        assert "showString' result" in prepared

    def test_prepare_code_boolean_output(self):
        """Test preparing code with boolean output."""
        code = """isEven :: Int -> Bool
isEven n = n `mod` 2 == 0"""

        test_case = {
            "function_name": "isEven",
            "parameters": {"n": 4},
            "expected": True,
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert (
            'if result then "true" else "false"' in prepared
            or 'if result then "true" else "false"' in prepared
        )

    def test_prepare_code_inplace_mode_1(self):
        """Test preparing code for inplace mode 1 (simulated)."""
        code = """sortList :: [Int] -> [Int]
sortList [] = []
sortList (x:xs) = sortList [y | y <- xs, y <= x] ++ [x] ++ sortList [y | y <- xs, y > x]"""

        test_case = {
            "function_name": "sortList",
            "parameters": {"lst": [3, 1, 4, 1, 5]},
            "expected": [1, 1, 3, 4, 5],
            "inplace": "1",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "let result = sortList lst" in prepared
        assert "showList' result" in prepared

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_success(self, mock_mkdtemp, mock_run):
        """Test successful test execution."""
        mock_mkdtemp.return_value = "/tmp/test"

        # Mock compilation success
        compile_result = MagicMock(returncode=0, stderr="")
        # Mock execution success with integer output
        exec_result = MagicMock(returncode=0, stdout="8\n", stderr="")
        mock_run.side_effect = [compile_result, exec_result]

        code = """add :: Int -> Int -> Int
add x y = x + y"""

        test_case = {
            "function_name": "add",
            "parameters": {"x": 5, "y": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_string_output(self, mock_mkdtemp, mock_run):
        """Test execution with string output."""
        mock_mkdtemp.return_value = "/tmp/test"

        compile_result = MagicMock(returncode=0, stderr="")
        exec_result = MagicMock(returncode=0, stdout='"hello world"\n', stderr="")
        mock_run.side_effect = [compile_result, exec_result]

        code = """greet :: String -> String
greet name = "hello " ++ name"""

        test_case = {
            "function_name": "greet",
            "parameters": {"name": "world"},
            "expected": "hello world",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == "hello world"

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_list_output(self, mock_mkdtemp, mock_run):
        """Test execution with list output."""
        mock_mkdtemp.return_value = "/tmp/test"

        compile_result = MagicMock(returncode=0, stderr="")
        exec_result = MagicMock(returncode=0, stdout="[1, 2, 3, 4]\n", stderr="")
        mock_run.side_effect = [compile_result, exec_result]

        code = """range :: Int -> Int -> [Int]
range a b = [a..b]"""

        test_case = {
            "function_name": "range",
            "parameters": {"a": 1, "b": 4},
            "expected": [1, 2, 3, 4],
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3, 4]

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_compilation_error(self, mock_mkdtemp, mock_run):
        """Test handling compilation errors."""
        mock_mkdtemp.return_value = "/tmp/test"

        compile_result = MagicMock(
            returncode=1, stderr="test.hs:2:1: error: parse error on input 'add'"
        )
        mock_run.return_value = compile_result

        code = """add :: Int -> Int -> Int
add x y = """  # Invalid syntax

        test_case = {
            "function_name": "add",
            "parameters": {"x": 5, "y": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is False
        assert "Compilation failed" in result["error"]
        assert result["actual"] is None

    @patch("subprocess.run")
    @patch("tempfile.mkdtemp")
    def test_execute_test_runtime_error(self, mock_mkdtemp, mock_run):
        """Test handling runtime errors."""
        mock_mkdtemp.return_value = "/tmp/test"

        compile_result = MagicMock(returncode=0, stderr="")
        exec_result = MagicMock(
            returncode=1, stdout="", stderr="*** Exception: divide by zero"
        )
        mock_run.side_effect = [compile_result, exec_result]

        code = """divide :: Int -> Int -> Int
divide x y = x `div` y"""

        test_case = {
            "function_name": "divide",
            "parameters": {"x": 10, "y": 0},
            "expected": 0,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is False
        assert "Runtime error" in result["error"]

    def test_cleanup(self):
        """Test cleanup of temporary files."""
        import tempfile
        import os

        # Create a real temporary directory
        self.executor.temp_dir = tempfile.mkdtemp()
        temp_file = os.path.join(self.executor.temp_dir, "test.hs")

        # Create a file in the directory
        with open(temp_file, "w") as f:
            f.write("test")

        assert os.path.exists(self.executor.temp_dir)
        assert os.path.exists(temp_file)

        # Clean up
        self.executor.cleanup()

        # Directory should be removed
        assert not os.path.exists(self.executor.temp_dir)
