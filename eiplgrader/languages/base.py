"""Abstract base classes for language adapters and executors."""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional, Tuple, Union
from dataclasses import dataclass
import re


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


# Component interfaces for composition pattern
class PromptGenerator(ABC):
    """Component interface for generating language-specific prompts."""
    
    @abstractmethod
    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate language-specific prompt for LLM."""
        pass


class CodeExtractor(ABC):
    """Component interface for extracting code from LLM responses."""
    
    @abstractmethod
    def extract_code(self, llm_response: str) -> List[str]:
        """Extract code blocks from LLM response."""
        pass
    
    @abstractmethod
    def extract_functions(self, code: str) -> List[Dict[str, Any]]:
        """Extract function definitions from code with metadata."""
        pass


class CodeNormalizer(ABC):
    """Component interface for normalizing code for comparison."""
    
    @abstractmethod
    def normalize_code(self, code: str) -> str:
        """Normalize code for comparison (remove comments, whitespace, etc.)."""
        pass


class UnifiedLanguageAdapter(ABC):
    """Enhanced abstract base for unified language adapters with deduplication support.
    
    This class extends the basic LanguageAdapter interface with additional methods
    needed for advanced language-specific processing, validation strategies,
    and deduplication features. It uses composition with PromptGenerator,
    CodeExtractor, and ValidationStrategy components.
    """

    def __init__(self):
        self._prompt_generator: Optional[PromptGenerator] = None
        self._code_extractor: Optional[CodeExtractor] = None
        self._code_normalizer: Optional[CodeNormalizer] = None
        self._validation_strategy = None  # Will be set by subclasses

    @abstractmethod
    def get_spec(self) -> 'LanguageSpec':  # Forward reference to avoid circular import
        """Return enhanced language specification"""
        pass

    def generate_prompt(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Generate language-specific prompt for LLM using composition."""
        if self._prompt_generator:
            return self._prompt_generator.generate_prompt(
                student_response, function_name, gen_type, **kwargs
            )
        else:
            # Fallback to abstract method for subclasses that don't use composition
            return self._generate_prompt_impl(student_response, function_name, gen_type, **kwargs)

    @abstractmethod
    def _generate_prompt_impl(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Implementation method for prompt generation."""
        pass

    def extract_code(self, llm_response: str) -> List[str]:
        """Extract code blocks from LLM response using composition."""
        if self._code_extractor:
            return self._code_extractor.extract_code(llm_response)
        else:
            # Fallback to abstract method
            return self._extract_code_impl(llm_response)

    @abstractmethod
    def _extract_code_impl(self, llm_response: str) -> List[str]:
        """Implementation method for code extraction."""
        pass

    def validate_syntax(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate syntax using the configured validation strategy."""
        if self._validation_strategy:
            return self._validation_strategy.validate(code)
        else:
            # Fallback to abstract method
            return self._validate_syntax_impl(code)

    @abstractmethod
    def _validate_syntax_impl(self, code: str) -> Tuple[bool, Optional[str]]:
        """Implementation method for syntax validation."""
        pass

    def extract_functions(self, code: str) -> List[Dict[str, Any]]:
        """Extract function definitions from code with metadata."""
        if self._code_extractor:
            return self._code_extractor.extract_functions(code)
        else:
            # Fallback to abstract method
            return self._extract_functions_impl(code)

    @abstractmethod
    def _extract_functions_impl(self, code: str) -> List[Dict[str, Any]]:
        """Implementation method for function extraction.
        
        Returns list of dictionaries with keys:
        - 'name': function name
        - 'code': function code block
        - 'signature': function signature
        - 'start_line': starting line number (if available)
        - 'end_line': ending line number (if available)
        """
        pass

    def normalize_code(self, code: str) -> str:
        """Normalize code for comparison using composition."""
        if self._code_normalizer:
            return self._code_normalizer.normalize_code(code)
        else:
            # Fallback to abstract method
            return self._normalize_code_impl(code)

    @abstractmethod
    def _normalize_code_impl(self, code: str) -> str:
        """Implementation method for code normalization."""
        pass

    # Component setters for dependency injection
    def set_prompt_generator(self, generator: PromptGenerator) -> None:
        """Set the prompt generator component."""
        self._prompt_generator = generator

    def set_code_extractor(self, extractor: CodeExtractor) -> None:
        """Set the code extractor component."""
        self._code_extractor = extractor

    def set_code_normalizer(self, normalizer: CodeNormalizer) -> None:
        """Set the code normalizer component."""
        self._code_normalizer = normalizer

    def set_validation_strategy(self, strategy) -> None:  # Type hint avoided to prevent circular import
        """Set the validation strategy component."""
        self._validation_strategy = strategy

    def get_config(self) -> LanguageConfig:
        """Return basic language configuration for backward compatibility"""
        spec = self.get_spec()
        return LanguageConfig(
            name=spec.name,
            display_name=spec.display_name,
            file_extensions=spec.file_extensions,
            run_command=spec.run_command,
            docker_image=spec.docker_image,
            compile_command=spec.compile_command,
            test_timeout=spec.test_timeout,
        )


class BaseUnifiedAdapter(UnifiedLanguageAdapter):
    """Base implementation of UnifiedLanguageAdapter with common functionality.
    
    This class provides default implementations for common operations
    and can be extended by specific language adapters.
    """
    
    def _extract_code_with_patterns(self, llm_response: str, patterns: List[str]) -> List[str]:
        """Extract code using multiple regex patterns."""
        for pattern in patterns:
            matches = re.findall(pattern, llm_response, re.DOTALL)
            if matches:
                return [match.strip() for match in matches]
        
        # If no patterns match, return the response as-is
        return [llm_response.strip()] if llm_response.strip() else []
    
    def _extract_functions_with_pattern(self, code: str, pattern: str, name_group: int = 1) -> List[Dict[str, Any]]:
        """Extract functions using regex pattern."""
        functions = []
        lines = code.split('\n')
        
        for i, line in enumerate(lines):
            match = re.search(pattern, line)
            if match:
                func_name = match.group(name_group)
                # Simple function extraction - can be enhanced
                func_dict = {
                    'name': func_name,
                    'signature': line.strip(),
                    'start_line': i + 1,
                    'code': line.strip(),  # Simplified - should extract full function
                }
                functions.append(func_dict)
        
        return functions
    
    def _normalize_basic(self, code: str) -> str:
        """Basic code normalization."""
        # Remove comments (simplified)
        spec = self.get_spec()
        conventions = spec.syntax_conventions
        
        if conventions.comment_single:
            code = re.sub(f'{re.escape(conventions.comment_single)}.*', '', code)
        
        # Remove extra whitespace
        code = re.sub(r'\s+', ' ', code)
        code = code.strip()
        
        return code


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


# Type alias for backward compatibility
AdapterType = Union[LanguageAdapter, UnifiedLanguageAdapter]
