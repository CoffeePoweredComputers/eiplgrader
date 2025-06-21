"""Simple base classes for language adapters."""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Optional


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
        raise NotImplementedError

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
        raise NotImplementedError

    @abstractmethod
    def extract_code(self, llm_response: str) -> List[str]:
        """Extract code blocks from LLM response."""
        raise NotImplementedError

    @abstractmethod
    def normalize_code(self, code: str) -> str:
        """Normalize code by removing comments and standardizing format."""
        raise NotImplementedError
