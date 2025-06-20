"""Simple base classes for language adapters."""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Optional, Dict, Any, Union
import subprocess
import tempfile
import os


@dataclass
class LanguageConfig:
    """Configuration for a language implementation."""

    name: str
    display_name: str
    file_extensions: List[str]
    run_command: List[str]
    compile_command: Optional[List[str]] = None
    test_timeout: int = 30


class LanguageAdapter(ABC):
    """Abstract base class for language-specific code generation."""

    @abstractmethod
    def get_config(self) -> LanguageConfig:
        """Return language configuration."""
        pass

    @abstractmethod
    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        num_to_gen: int = 1,
        **kwargs,
    ) -> str:
        """Generate language-specific prompt for LLM."""
        pass

    @abstractmethod
    def extract_code(self, llm_response: str) -> List[str]:
        """Extract code blocks from LLM response."""
        pass

    @abstractmethod
    def normalize_code(self, code: str) -> str:
        """Normalize code by removing comments and standardizing format."""
        pass


class LanguageExecutor(ABC):
    """Abstract base class for language-specific code execution."""

    @abstractmethod
    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare code for execution with test harness."""
        pass

    @abstractmethod
    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute code with test case and return results."""
        pass

    @abstractmethod
    def cleanup(self) -> None:
        """Clean up any temporary resources."""
        pass

    def validate_types_provided(self, test_case: Dict[str, Any]) -> None:
        """Validate that required type information is provided.
        Raises ValueError if types are missing.
        """
        errors = []
        if "parameter_types" not in test_case:
            errors.append("parameter_types not provided")
        if "expected_type" not in test_case:
            errors.append("expected_type not provided")

        if errors:
            error_msg = "Missing required type information:\n"
            for error in errors:
                error_msg += f"- {error}\n"
            error_msg += "\nTest case must include:\n"
            error_msg += "{\n"
            error_msg += '    "parameters": {...},\n'
            error_msg += '    "parameter_types": {"param1": "type1", ...},\n'
            error_msg += '    "expected": ...,\n'
            error_msg += '    "expected_type": "type"\n'
            error_msg += "}"
            raise ValueError(error_msg)

        # Validate all parameters have types
        parameters = test_case.get("parameters", {})
        parameter_types = test_case.get("parameter_types", {})
        for param_name in parameters:
            if param_name not in parameter_types:
                raise ValueError(
                    f"Missing required type information:\n- parameter_types['{param_name}'] not provided"
                )

    def infer_type(self, value: Any) -> str:
        """Infer type from a Python value."""
        if isinstance(value, bool):
            return "bool"
        elif isinstance(value, int):
            return "int"
        elif isinstance(value, float):
            return "double"
        elif isinstance(value, str):
            return "string"
        elif isinstance(value, list):
            if value and isinstance(value[0], int):
                return "List[int]"
            elif value and isinstance(value[0], float):
                return "List[double]"
            elif value and isinstance(value[0], str):
                return "List[string]"
            return "List"
        return "unknown"

    def format_array_output(self, arr: List[Any], language: str) -> str:
        """Format array output for different languages."""
        if language in ["python", "go"]:
            return str(arr)
        elif language == "javascript":
            return str(arr).replace("'", '"')
        elif language == "java":
            return str(arr).replace("'", "")
        elif language in ["c", "cpp"]:
            return "{" + ", ".join(str(x) for x in arr) + "}"
        elif language == "haskell":
            return str(arr)
        return str(arr)


class InterpretedLanguageExecutor(LanguageExecutor):
    """Base class for interpreted languages (Python, JavaScript)."""
    
    def __init__(self):
        self.requires_types = False
    
    def validate_or_infer_types(self, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Validate types if provided, or infer them from values."""
        # For interpreted languages, types are optional
        if "parameter_types" not in test_case:
            test_case["parameter_types"] = {}
            for param_name, value in test_case.get("parameters", {}).items():
                test_case["parameter_types"][param_name] = self.infer_type(value)
        
        if "expected_type" not in test_case:
            test_case["expected_type"] = self.infer_type(test_case.get("expected"))
        
        return test_case


class CompiledLanguageExecutor(LanguageExecutor):
    """Base class for compiled languages (C, C++, Java, Go, Haskell)."""
    
    def __init__(self):
        self.requires_types = True
        self.temp_dir = tempfile.mkdtemp()
    
    def cleanup(self) -> None:
        """Clean up temporary directory."""
        if hasattr(self, 'temp_dir') and os.path.exists(self.temp_dir):
            for file in os.listdir(self.temp_dir):
                os.remove(os.path.join(self.temp_dir, file))
            os.rmdir(self.temp_dir)
    
    def get_type_mapping(self, type_str: str, language: str) -> str:
        """Map generic type strings to language-specific types."""
        mappings = {
            "c": {
                "int": "int",
                "double": "double",
                "float": "float",
                "string": "char*",
                "bool": "int",
                "int[]": "int*",
                "double[]": "double*",
                "float[]": "float*",
            },
            "cpp": {
                "int": "int",
                "double": "double",
                "float": "float",
                "string": "std::string",
                "bool": "bool",
                "List[int]": "std::vector<int>",
                "std::vector<int>": "std::vector<int>",
                "int[]": "std::vector<int>",
            },
            "java": {
                "int": "int",
                "double": "double",
                "float": "float",
                "string": "String",
                "bool": "boolean",
                "int[]": "int[]",
                "double[]": "double[]",
                "List[int]": "int[]",
            },
            "go": {
                "int": "int",
                "double": "float64",
                "float": "float32",
                "string": "string",
                "bool": "bool",
                "int[]": "[]int",
                "List[int]": "[]int",
            },
            "haskell": {
                "int": "Int",
                "double": "Double",
                "float": "Float",
                "string": "String",
                "bool": "Bool",
                "List[int]": "[Int]",
                "int[]": "[Int]",
            }
        }
        
        return mappings.get(language, {}).get(type_str, type_str)
    
    def format_value(self, value: Any, type_str: str, language: str) -> str:
        """Format a value for embedding in code."""
        if isinstance(value, bool):
            if language == "c":
                return "1" if value else "0"
            elif language == "haskell":
                return "True" if value else "False"
            else:
                return "true" if value else "false"
        elif isinstance(value, str):
            return f'"{value}"'
        elif isinstance(value, list):
            if language == "c":
                # C doesn't support dynamic arrays easily
                return "{" + ", ".join(str(x) for x in value) + "}"
            elif language == "cpp":
                return "{" + ", ".join(str(x) for x in value) + "}"
            elif language == "java":
                return "{" + ", ".join(str(x) for x in value) + "}"
            elif language == "go":
                elem_type = self.get_type_mapping(type_str, language)
                return f"{elem_type}{{{', '.join(str(x) for x in value)}}}"
            elif language == "haskell":
                return str(value)
        return str(value)
