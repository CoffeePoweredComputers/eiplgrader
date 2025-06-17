"""Enhanced language specification for unified language adapters."""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Protocol, runtime_checkable
from abc import ABC, abstractmethod

from .base import LanguageConfig


@dataclass
class SyntaxConventions:
    """Language-specific syntax conventions and patterns."""
    
    comment_single: str = ""  # Single line comment prefix (e.g., "#", "//")
    comment_multi_start: str = ""  # Multi-line comment start (e.g., "/*")
    comment_multi_end: str = ""  # Multi-line comment end (e.g., "*/")
    string_delimiters: List[str] = field(default_factory=lambda: ['"', "'"])
    block_start: str = "{"  # Block start character
    block_end: str = "}"  # Block end character
    statement_terminator: str = ""  # Statement terminator (e.g., ";")
    indentation_type: str = "spaces"  # "spaces" or "tabs"
    indentation_size: int = 4
    case_sensitive: bool = True
    supports_multiline_strings: bool = True


@dataclass
class FunctionPatterns:
    """Language-specific function definition patterns."""
    
    definition_regex: str  # Regex pattern to match function definitions
    name_capture_group: int = 1  # Capture group for function name
    signature_capture_group: int = 0  # Capture group for full signature
    body_capture_group: Optional[int] = None  # Capture group for function body
    requires_return_type: bool = False
    supports_overloading: bool = False
    supports_default_params: bool = True
    supports_varargs: bool = True


@dataclass
class HeaderRequirements:
    """Language-specific header/import requirements."""
    
    required_imports: List[str] = field(default_factory=list)
    optional_imports: List[str] = field(default_factory=list)
    import_template: str = ""  # Template for import statements
    include_guard_template: str = ""  # For languages like C/C++
    namespace_template: str = ""  # For languages with namespaces
    package_declaration: str = ""  # For languages like Java


@dataclass
class TemplateOverrides:
    """Custom templates for specific generation types."""
    
    cgbg_template: Optional[str] = None  # Code generation based grading
    redef_template: Optional[str] = None  # Function redefinition
    segmentation_template: Optional[str] = None  # Code segmentation
    custom_templates: Dict[str, str] = field(default_factory=dict)


@runtime_checkable
class ValidationStrategy(Protocol):
    """Protocol for validation strategies."""
    
    def validate(self, code: str) -> tuple[bool, Optional[str]]:
        """Validate code and return (is_valid, error_message)."""
        ...


@dataclass
class LanguageSpec:
    """Enhanced language specification that extends LanguageConfig.
    
    This dataclass provides comprehensive configuration for language adapters,
    including syntax conventions, function patterns, validation strategies,
    and template overrides for advanced language processing.
    """
    
    # Core configuration (extends LanguageConfig)
    name: str
    display_name: str
    file_extensions: List[str]
    run_command: List[str]
    docker_image: Optional[str] = None
    compile_command: Optional[List[str]] = None
    test_timeout: int = 30
    
    # Enhanced specification fields
    code_block_tag: str = ""  # Markdown code block tag (e.g., "python", "c")
    student_model_template: str = ""  # Base student model prompt template
    syntax_conventions: SyntaxConventions = field(default_factory=SyntaxConventions)
    function_patterns: Optional[FunctionPatterns] = None
    header_requirements: HeaderRequirements = field(default_factory=HeaderRequirements)
    
    # Validation configuration
    validation_strategy: str = "compiler"  # "compiler", "parser", "custom", "none"
    validation_command: Optional[List[str]] = None  # Command for validation
    validation_flags: List[str] = field(default_factory=list)  # Additional flags
    
    # Template overrides
    template_overrides: TemplateOverrides = field(default_factory=TemplateOverrides)
    
    # Additional metadata
    supports_segmentation: bool = True
    supports_multiple_functions: bool = True
    requires_main_wrapper: bool = False  # For compiled languages
    normalization_rules: Dict[str, Any] = field(default_factory=dict)
    
    def to_language_config(self) -> LanguageConfig:
        """Convert to basic LanguageConfig for backward compatibility."""
        return LanguageConfig(
            name=self.name,
            display_name=self.display_name,
            file_extensions=self.file_extensions,
            run_command=self.run_command,
            docker_image=self.docker_image,
            compile_command=self.compile_command,
            test_timeout=self.test_timeout,
        )
    
    def get_code_block_tag(self) -> str:
        """Get the code block tag, defaulting to language name if not set."""
        return self.code_block_tag or self.name
    
    def get_validation_command(self) -> Optional[List[str]]:
        """Get the validation command with flags."""
        if not self.validation_command:
            return None
        return self.validation_command + self.validation_flags
    
    def supports_validation_strategy(self, strategy: str) -> bool:
        """Check if the language supports a specific validation strategy."""
        supported_strategies = {
            "compiler": bool(self.compile_command or self.validation_command),
            "parser": True,  # All languages support basic parsing
            "custom": True,  # Custom validation can be implemented for any language
            "none": True,
        }
        return supported_strategies.get(strategy, False)


# Pre-defined function patterns for common languages
PYTHON_FUNCTION_PATTERN = FunctionPatterns(
    definition_regex=r"def\s+(\w+)\s*\([^)]*\):",
    name_capture_group=1,
    supports_default_params=True,
    supports_varargs=True,
)

C_FUNCTION_PATTERN = FunctionPatterns(
    definition_regex=r"(\w+\s+)?(\w+)\s*\([^)]*\)\s*{",
    name_capture_group=2,
    requires_return_type=True,
    supports_overloading=False,
    supports_default_params=False,
)

JAVA_FUNCTION_PATTERN = FunctionPatterns(
    definition_regex=r"(public|private|protected)?\s*(static)?\s*(\w+)\s+(\w+)\s*\([^)]*\)\s*{",
    name_capture_group=4,
    requires_return_type=True,
    supports_overloading=True,
)

JAVASCRIPT_FUNCTION_PATTERN = FunctionPatterns(
    definition_regex=r"function\s+(\w+)\s*\([^)]*\)\s*{",
    name_capture_group=1,
    supports_default_params=True,
    supports_varargs=True,
)

# Pre-defined syntax conventions
PYTHON_SYNTAX = SyntaxConventions(
    comment_single="#",
    block_start=":",
    block_end="",
    statement_terminator="",
    indentation_type="spaces",
    indentation_size=4,
)

C_SYNTAX = SyntaxConventions(
    comment_single="//",
    comment_multi_start="/*",
    comment_multi_end="*/",
    statement_terminator=";",
    indentation_type="spaces",
    indentation_size=4,
)

JAVA_SYNTAX = SyntaxConventions(
    comment_single="//",
    comment_multi_start="/*",
    comment_multi_end="*/",
    statement_terminator=";",
    indentation_type="spaces",
    indentation_size=4,
)