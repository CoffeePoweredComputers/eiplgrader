"""Tests for TypeScript language adapter and executor."""

import os
import pytest
from unittest.mock import patch, MagicMock
from eiplgrader.languages.adapters.typescript_adapter import TypescriptAdapter
from eiplgrader.languages.executors.typescript_executor import TypescriptExecutor


class TestTypescriptAdapter:
    """Test cases for TypeScript adapter."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = TypescriptAdapter()

    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "typescript"
        assert config.display_name == "TypeScript"
        assert config.file_extensions == [".ts"]
        assert config.run_command == ["ts-node"]
        assert config.compile_command == ["tsc"]

    def test_generate_prompt_cgbg(self):
        """Test prompt generation for code generation based grading."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg",
        )

        assert "TypeScript" in prompt
        assert "add" in prompt
        assert "adds two numbers together" in prompt
        assert "type annotations" in prompt
        assert "```typescript" in prompt

    def test_generate_prompt_redef(self):
        """Test prompt generation for function redefinition."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="multiply",
            gen_type="redef",
            params="x: number, y: number",
            assumptions="x and y are numbers",
        )

        assert "multiply" in prompt
        assert "x: number, y: number" in prompt
        assert "x and y are numbers" in prompt
        assert "type annotations" in prompt

    def test_generate_prompt_multiple_versions(self):
        """Test prompt generation with multiple versions."""
        prompt = self.adapter.generate_prompt(
            student_response="calculates factorial",
            function_name="factorial",
            gen_type="cgbg",
            num_to_gen=3,
        )

        assert "3 different versions" in prompt

    def test_extract_code_markdown(self):
        """Test extracting code from markdown blocks."""
        llm_response = """Here's the function:
        
```typescript
function add(a: number, b: number): number {
    return a + b;
}
```

And another version:

```typescript
const add = (a: number, b: number): number => a + b;
```
"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 2
        assert "function add(a: number, b: number): number" in code_blocks[0]
        assert "const add = (a: number, b: number): number =>" in code_blocks[1]

    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        llm_response = """function multiply(x: number, y: number): number {
    return x * y;
}"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 1
        assert "function multiply" in code_blocks[0]

    def test_extract_code_arrow_function(self):
        """Test extracting arrow functions."""
        llm_response = """const square = (n: number): number => n * n;"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 1
        assert "const square" in code_blocks[0]

    def test_extract_code_async_function(self):
        """Test extracting async functions."""
        llm_response = """async function fetchData(url: string): Promise<string> {
    const response = await fetch(url);
    return response.text();
}"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 1
        assert "async function fetchData" in code_blocks[0]

    @patch("subprocess.run")
    def test_validate_syntax_valid(self, mock_run):
        """Test syntax validation with valid code."""
        mock_run.return_value = MagicMock(returncode=0)

        valid, error = self.adapter.validate_syntax("function test(): void {}")
        assert valid is True
        assert error is None

    @patch("subprocess.run")
    def test_validate_syntax_invalid(self, mock_run):
        """Test syntax validation with invalid code."""
        mock_run.return_value = MagicMock(
            returncode=1, stdout="error TS1005: ';' expected."
        )

        valid, error = self.adapter.validate_syntax("function test() {")
        assert valid is False
        assert "TypeScript error" in error
        assert "TS1005" in error


class TestTypescriptExecutor:
    """Test cases for TypeScript executor."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = TypescriptExecutor()

    def teardown_method(self):
        """Clean up after tests."""
        self.executor.cleanup()

    def test_prepare_code_normal_mode(self):
        """Test code preparation for normal mode."""
        code = "function add(a: number, b: number): number { return a + b; }"
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)
        assert "function add" in prepared
        assert 'const testParams = {"a": 5, "b": 3}' in prepared
        assert 'inplaceMode === "0"' in prepared
        assert "result = await add(...args)" in prepared

    def test_prepare_code_inplace_mode(self):
        """Test code preparation for in-place mode."""
        code = "function sort(arr: number[]): void { arr.sort((a, b) => a - b); }"
        test_case = {
            "function_name": "sort",
            "parameters": {"arr": [3, 1, 2]},
            "inplace": "1",
        }

        prepared = self.executor.prepare_code(code, test_case)
        assert 'inplaceMode === "1"' in prepared
        assert "JSON.parse(JSON.stringify(args[0]))" in prepared

    def test_prepare_code_with_types(self):
        """Test that TypeScript types are preserved in prepared code."""
        code = """
interface Point {
    x: number;
    y: number;
}

function distance(p1: Point, p2: Point): number {
    return Math.sqrt((p2.x - p1.x) ** 2 + (p2.y - p1.y) ** 2);
}
"""
        test_case = {
            "function_name": "distance",
            "parameters": {"p1": {"x": 0, "y": 0}, "p2": {"x": 3, "y": 4}},
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)
        assert "interface Point" in prepared
        assert "function distance(p1: Point, p2: Point): number" in prepared

    @patch("subprocess.run")
    def test_execute_test_success(self, mock_run):
        """Test successful code execution."""
        # Mock successful compilation and execution
        mock_run.side_effect = [
            # tsc compilation
            MagicMock(returncode=0, stdout="", stderr=""),
            # node execution
            MagicMock(returncode=0, stdout="8", stderr=""),
        ]

        code = "function add(a: number, b: number): number { return a + b; }"
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8
        assert "function_call" in result
        assert result["function_call"] == "add(5, 3)"

    @patch("subprocess.run")
    def test_execute_test_compilation_error(self, mock_run):
        """Test handling of compilation errors."""
        # Mock compilation failure
        mock_run.return_value = MagicMock(
            returncode=1,
            stdout="error TS2322: Type 'string' is not assignable to type 'number'.",
            stderr="",
        )

        code = "function add(a: number, b: number): number { return 'invalid'; }"
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is False
        assert "TypeScript compilation failed" in result["error"]
        assert "TS2322" in result["error"]

    @patch("subprocess.run")
    def test_execute_test_runtime_error(self, mock_run):
        """Test handling of runtime errors."""
        # Mock successful compilation but runtime error
        mock_run.side_effect = [
            # tsc compilation
            MagicMock(returncode=0, stdout="", stderr=""),
            # node execution with error
            MagicMock(
                returncode=1,
                stdout="",
                stderr='{"error": "Cannot read property \'length\' of undefined"}',
            ),
        ]

        code = "function getLength(arr: any[]): number { return arr.length; }"
        test_case = {
            "function_name": "getLength",
            "parameters": {"arr": None},
            "expected": 0,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is False
        assert "Cannot read property 'length' of undefined" in result["error"]

    @patch("subprocess.run")
    def test_execute_test_with_ts_node_fallback(self, mock_run):
        """Test fallback to ts-node when tsc is not available."""
        # Mock successful ts-node execution
        mock_run.return_value = MagicMock(returncode=0, stdout="10", stderr="")

        # Patch the compile_typescript method to simulate tsc not found
        with patch.object(
            self.executor,
            "compile_typescript",
            return_value=(True, os.path.join(self.executor.temp_dir, "test.ts"), ""),
        ):
            code = "function multiply(x: number, y: number): number { return x * y; }"
            test_case = {
                "function_name": "multiply",
                "parameters": {"x": 2, "y": 5},
                "expected": 10,
                "inplace": "0",
            }

            result = self.executor.execute_test(code, test_case)
            assert result["passed"] is True
            assert result["actual"] == 10

    @patch("subprocess.run")
    def test_execute_test_timeout(self, mock_run):
        """Test handling of execution timeout."""
        import subprocess

        # Mock timeout
        mock_run.side_effect = subprocess.TimeoutExpired(cmd="node", timeout=30)

        code = "function infinite(): void { while(true) {} }"
        test_case = {
            "function_name": "infinite",
            "parameters": {},
            "expected": None,
            "inplace": "0",
            "timeout": 1,
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is False
        assert "timeout" in result["error"].lower()

    def test_cleanup(self):
        """Test cleanup of temporary files."""
        # Ensure temp directory exists
        assert os.path.exists(self.executor.temp_dir)

        # Clean up
        self.executor.cleanup()

        # Verify cleanup
        assert not os.path.exists(self.executor.temp_dir)
