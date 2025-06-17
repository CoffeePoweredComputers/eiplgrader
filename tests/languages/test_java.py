"""Tests for Java language support."""
import pytest
from unittest.mock import patch, MagicMock
from eiplgrader.languages.adapters.java_adapter import JavaAdapter
from eiplgrader.languages.executors.java_executor import JavaExecutor


class TestJavaAdapter:
    """Test Java adapter functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = JavaAdapter()
    
    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "java"
        assert config.display_name == "Java"
        assert config.file_extensions == [".java"]
        assert config.run_command == ["java"]
        assert config.compile_command == ["javac"]
    
    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg"
        )
        
        assert "introductory CS student" in prompt
        assert "Java" in prompt
        assert "add" in prompt
        assert "adds two numbers together" in prompt
        assert "public class Solution" in prompt
        assert "```java" in prompt
    
    def test_generate_prompt_redef(self):
        """Test function redefinition prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="multiply",
            gen_type="redef",
            params="int a, int b",
            assumptions="Both arguments are positive integers"
        )
        
        assert "multiply" in prompt
        assert "int a, int b" in prompt
        assert "positive integers" in prompt
        assert "public static" in prompt
        assert "Solution" in prompt
    
    def test_generate_prompt_multiple_versions(self):
        """Test prompt with multiple versions requested."""
        prompt = self.adapter.generate_prompt(
            student_response="reverses a string",
            function_name="reverse",
            gen_type="cgbg",
            num_to_gen=3
        )
        
        assert "3 different versions" in prompt
        assert "Java idioms" in prompt
    
    def test_extract_code_markdown_blocks(self):
        """Test extracting code from markdown blocks."""
        response = """Here's the solution:
        
```java
public class Solution {
    public static int add(int a, int b) {
        return a + b;
    }
}
```

And another version:

```java
public class Solution {
    public static int add(int x, int y) {
        return x + y;
    }
}
```"""
        
        codes = self.adapter.extract_code(response)
        assert len(codes) == 2
        assert "public class Solution" in codes[0]
        assert "return a + b" in codes[0]
        assert "return x + y" in codes[1]
    
    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        response = """public class Solution {
    public static String reverse(String s) {
        return new StringBuilder(s).reverse().toString();
    }
}"""
        
        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert "public class Solution" in codes[0]
        assert "reverse" in codes[0]
    
    def test_extract_code_standalone_method(self):
        """Test extracting standalone method and wrapping in class."""
        response = """public static int multiply(int a, int b) {
    return a * b;
}"""
        
        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert "public class Solution" in codes[0]
        assert "multiply" in codes[0]
        assert "return a * b" in codes[0]
    
    def test_validate_syntax_valid(self):
        """Test syntax validation for valid code."""
        code = """public class Solution {
    public static int add(int a, int b) {
        return a + b;
    }
}"""
        
        is_valid, error = self.adapter.validate_syntax(code)
        assert is_valid
        assert error is None
    
    def test_validate_syntax_unbalanced_braces(self):
        """Test syntax validation for unbalanced braces."""
        code = """public class Solution {
    public static int add(int a, int b) {
        return a + b;
    }"""
        
        is_valid, error = self.adapter.validate_syntax(code)
        assert not is_valid
        assert "Unbalanced braces" in error
    
    def test_validate_syntax_no_class(self):
        """Test syntax validation when no class found."""
        code = """public static int add(int a, int b) {
    return a + b;
}"""
        
        is_valid, error = self.adapter.validate_syntax(code)
        assert not is_valid
        assert "No class definition found" in error
    
    def test_validate_syntax_no_method(self):
        """Test syntax validation when no method found."""
        code = """public class Solution {
    // Empty class
}"""
        
        is_valid, error = self.adapter.validate_syntax(code)
        assert not is_valid
        assert "No method definition found" in error


class TestJavaExecutor:
    """Test Java executor functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.executor = JavaExecutor()
    
    def test_prepare_code_simple_function(self):
        """Test preparing code for a simple function."""
        code = """public class Solution {
    public static int add(int a, int b) {
        return a + b;
    }
}"""
        
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8
        }
        
        prepared = self.executor.prepare_code(code, test_case)
        
        assert "public class Test" in prepared
        assert "Solution.add" in prepared
        assert "Integer.parseInt(args[0])" in prepared
        assert "Integer.parseInt(args[1])" in prepared
        assert "toJson(result)" in prepared
    
    def test_prepare_code_array_parameter(self):
        """Test preparing code with array parameters."""
        code = """public class Solution {
    public static int sum(int[] numbers) {
        int total = 0;
        for (int n : numbers) {
            total += n;
        }
        return total;
    }
}"""
        
        test_case = {
            "function_name": "sum",
            "parameters": {"numbers": [1, 2, 3, 4]},
            "expected": 10
        }
        
        prepared = self.executor.prepare_code(code, test_case)
        
        assert "split(\",\")" in prepared
        assert "int[] numbers" in prepared
        assert "Integer.parseInt" in prepared
    
    def test_prepare_code_inplace_mode(self):
        """Test preparing code for in-place modification."""
        code = """public class Solution {
    public static void reverse(int[] arr) {
        int left = 0, right = arr.length - 1;
        while (left < right) {
            int temp = arr[left];
            arr[left] = arr[right];
            arr[right] = temp;
            left++;
            right--;
        }
    }
}"""
        
        test_case = {
            "function_name": "reverse",
            "parameters": {"arr": [1, 2, 3, 4]},
            "expected": [4, 3, 2, 1],
            "inplace": "1"
        }
        
        prepared = self.executor.prepare_code(code, test_case)
        
        assert "Solution.reverse(arr)" in prepared
        assert "toJson(arr)" in prepared  # Should print the modified array
    
    def test_infer_java_type(self):
        """Test Java type inference."""
        assert self.executor._infer_java_type(True) == "boolean"
        assert self.executor._infer_java_type(42) == "int"
        assert self.executor._infer_java_type(3.14) == "double"
        assert self.executor._infer_java_type("hello") == "String"
        assert self.executor._infer_java_type([1, 2, 3]) == "int[]"
        assert self.executor._infer_java_type([1.0, 2.0]) == "double[]"
        assert self.executor._infer_java_type(["a", "b"]) == "String[]"
    
    @patch('subprocess.run')
    def test_compile_success(self, mock_run):
        """Test successful compilation."""
        mock_run.return_value = MagicMock(returncode=0, stderr="")
        
        success, output_path, error = self.executor.compile("/tmp/Test.java")
        
        assert success
        assert output_path == "/tmp"
        assert error == ""
    
    @patch('subprocess.run')
    def test_compile_failure(self, mock_run):
        """Test compilation failure."""
        mock_run.return_value = MagicMock(
            returncode=1, 
            stderr="Test.java:1: error: class Solution is public, should be declared in a file named Solution.java"
        )
        
        success, output_path, error = self.executor.compile("/tmp/Test.java")
        
        assert not success
        assert output_path == ""
        assert "error:" in error
    
    @patch('subprocess.run')
    @patch('os.path.exists')
    def test_execute_test_success(self, mock_exists, mock_run):
        """Test successful test execution."""
        # Mock file operations
        mock_exists.return_value = False  # No Gson available
        
        # Mock compilation success
        compile_result = MagicMock(returncode=0, stderr="")
        # Mock execution success
        execute_result = MagicMock(
            returncode=0, 
            stdout="8\n", 
            stderr=""
        )
        mock_run.side_effect = [compile_result, execute_result]
        
        code = """public class Solution {
    public static int add(int a, int b) {
        return a + b;
    }
}"""
        
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8
        }
        
        # Patch file writing
        with patch('builtins.open', create=True):
            result = self.executor.execute_test(code, test_case)
        
        assert result["passed"]
        assert result["actual"] == 8
        assert result["expected"] == 8
    
    @patch('subprocess.run')
    def test_execute_test_runtime_error(self, mock_run):
        """Test handling runtime errors."""
        # Mock compilation success
        compile_result = MagicMock(returncode=0, stderr="")
        # Mock execution failure
        execute_result = MagicMock(
            returncode=1, 
            stdout="", 
            stderr="Exception in thread \"main\" java.lang.ArrayIndexOutOfBoundsException"
        )
        mock_run.side_effect = [compile_result, execute_result]
        
        code = """public class Solution {
    public static int get(int[] arr) {
        return arr[10];  // Will cause error
    }
}"""
        
        test_case = {
            "function_name": "get",
            "parameters": {"arr": [1, 2, 3]},
            "expected": 3
        }
        
        # Patch file writing
        with patch('builtins.open', create=True):
            result = self.executor.execute_test(code, test_case)
        
        assert not result["passed"]
        assert "Runtime error" in result["error"]
        assert "ArrayIndexOutOfBoundsException" in result["error"]
    
    @patch('subprocess.run')
    def test_execute_test_timeout(self, mock_run):
        """Test handling execution timeout."""
        import subprocess
        
        # Mock compilation success
        compile_result = MagicMock(returncode=0, stderr="")
        mock_run.side_effect = [
            compile_result,
            subprocess.TimeoutExpired(cmd=["java", "Test"], timeout=30)
        ]
        
        code = """public class Solution {
    public static void loop() {
        while (true) {}  // Infinite loop
    }
}"""
        
        test_case = {
            "function_name": "loop",
            "parameters": {},
            "expected": None,
            "timeout": 1
        }
        
        # Patch file writing
        with patch('builtins.open', create=True):
            result = self.executor.execute_test(code, test_case)
        
        assert not result["passed"]
        assert "timeout" in result["error"].lower()
    
    def test_convert_to_simple_json(self):
        """Test conversion to simple JSON implementation."""
        code_with_gson = """import java.util.*;
import com.google.gson.Gson;

public class Test {
    private static String toJson(Object obj) {
        return new Gson().toJson(obj);
    }
    
    public static void main(String[] args) {
        System.out.println(toJson(42));
    }
}"""
        
        converted = self.executor._convert_to_simple_json(code_with_gson)
        
        assert "import com.google.gson.Gson" not in converted
        assert "new Gson()" not in converted
        assert "if (obj == null) return \"null\"" in converted
        assert "if (obj instanceof int[])" in converted
    
    def test_cleanup(self):
        """Test cleanup of temporary files."""
        import tempfile
        import os
        
        # Create a real temp directory
        temp_dir = tempfile.mkdtemp()
        self.executor.temp_dir = temp_dir
        
        # Create a test file
        test_file = os.path.join(temp_dir, "test.txt")
        with open(test_file, 'w') as f:
            f.write("test")
        
        assert os.path.exists(temp_dir)
        assert os.path.exists(test_file)
        
        # Cleanup
        self.executor.cleanup()
        
        assert not os.path.exists(temp_dir)
        assert not os.path.exists(test_file)