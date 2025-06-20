"""Simple base classes for language adapters."""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Optional, Dict, Any


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
        if "parameter_types" not in test_case:
            raise ValueError("Test case missing required 'parameter_types'")
        if "expected_type" not in test_case:
            raise ValueError("Test case missing required 'expected_type'")
