"""Abstract base classes for language adapters and executors."""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional, Tuple
from dataclasses import dataclass


@dataclass
class LanguageConfig:
    """Configuration for a language implementation"""

    name: str
    display_name: str
    file_extensions: List[str]
    run_command: List[str]
    docker_image: Optional[str] = None
    compile_command: Optional[List[str]] = None
    test_timeout: int = 30


class LanguageAdapter(ABC):
    """Abstract base for language-specific code generation"""

    @abstractmethod
    def get_config(self) -> LanguageConfig:
        """Return language configuration"""
        pass

    @abstractmethod
    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate language-specific prompt for LLM"""
        pass

    @abstractmethod
    def extract_code(self, llm_response: str) -> List[str]:
        """Extract code blocks from LLM response"""
        pass

    @abstractmethod
    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate syntax, return (is_valid, error_message)"""
        pass


class LanguageExecutor(ABC):
    """Abstract base for language-specific code execution"""

    @abstractmethod
    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare code for execution with test harness"""
        pass

    @abstractmethod
    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute code with test case, return results"""
        pass

    @abstractmethod
    def cleanup(self) -> None:
        """Clean up any temporary resources"""
        pass
