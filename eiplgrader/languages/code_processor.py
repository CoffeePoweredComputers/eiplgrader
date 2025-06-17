"""Language-specific code post-processing utilities.

This module provides post-processing utilities for extracted code,
handling language-specific requirements like header injection,
class wrapping, and code cleanup.
"""

import re
from typing import Dict, List, Optional, Callable, Union, Any
from abc import ABC, abstractmethod
from dataclasses import dataclass


@dataclass
class ProcessorConfig:
    """Configuration for a code processor."""

    name: str
    description: str
    applies_to: List[str]  # List of languages this processor applies to
    priority: int = 0  # Higher priority processors run first


class CodeProcessor(ABC):
    """Abstract base class for code processors."""

    @abstractmethod
    def process(self, code: str, language: str, **kwargs) -> str:
        """Process the code and return the modified version."""
        pass

    @abstractmethod
    def get_config(self) -> ProcessorConfig:
        """Get the processor configuration."""
        pass


class HeaderInjectionProcessor(CodeProcessor):
    """Processor for injecting necessary headers into C/C++ code."""

    def __init__(self):
        self.language_headers = {
            "c": ["#include <stdio.h>", "#include <stdlib.h>"],
            "cpp": ["#include <iostream>", "#include <vector>", "#include <string>"],
        }

    def process(self, code: str, language: str, **kwargs) -> str:
        """Inject headers if missing."""
        if language not in self.language_headers:
            return code

        if "#include" in code:
            return code

        headers = self.language_headers[language]
        header_block = "\n".join(headers) + "\n\n"
        return header_block + code

    def get_config(self) -> ProcessorConfig:
        return ProcessorConfig(
            name="header_injection",
            description="Inject necessary headers into C/C++ code",
            applies_to=["c", "cpp"],
            priority=10,
        )


class ClassWrappingProcessor(CodeProcessor):
    """Processor for wrapping methods in appropriate class structures."""

    def __init__(self):
        self.class_templates: Dict[str, str] = {
            "java": "public class Solution {{\n    {code}\n}}",
            "kotlin": "class Solution {{\n    {code}\n}}",
            "scala": "object Solution {{\n    {code}\n}}",
        }

    def process(self, code: str, language: str, **kwargs) -> str:
        """Wrap method in class if needed."""
        if language not in self.class_templates:
            return code

        # Check if code already has a class definition
        if language == "java" and re.search(r"(public\s+)?class\s+\w+", code):
            return code
        elif language in ["kotlin", "scala"] and re.search(
            r"(class|object)\s+\w+", code
        ):
            return code

        # For Java, wrap standalone methods
        if language == "java" and re.search(r"public\s+static\s+\w+\s+\w+\s*\(", code):
            template = self.class_templates[language]
            return template.format(code=code)

        return code

    def get_config(self) -> ProcessorConfig:
        return ProcessorConfig(
            name="class_wrapping",
            description="Wrap methods in appropriate class structures",
            applies_to=["java", "kotlin", "scala"],
            priority=5,
        )


class SemicolonCleanupProcessor(CodeProcessor):
    """Processor for cleaning up semicolons in languages that don't require them."""

    def process(self, code: str, language: str, **kwargs) -> str:
        """Remove trailing semicolons for SQL and other languages."""
        if language == "sql":
            code = code.strip()
            if code.endswith(";"):
                code = code[:-1].strip()
        return code

    def get_config(self) -> ProcessorConfig:
        return ProcessorConfig(
            name="semicolon_cleanup",
            description="Remove trailing semicolons where not needed",
            applies_to=["sql"],
            priority=1,
        )


class WhitespaceNormalizationProcessor(CodeProcessor):
    """Processor for normalizing whitespace and indentation."""

    def process(self, code: str, language: str, **kwargs) -> str:
        """Normalize whitespace and indentation."""
        if not code:
            return code

        # Remove leading/trailing whitespace
        code = code.strip()

        # Normalize line endings
        code = code.replace("\r\n", "\n").replace("\r", "\n")

        # Remove excessive blank lines (more than 2 consecutive)
        code = re.sub(r"\n{3,}", "\n\n", code)

        return code

    def get_config(self) -> ProcessorConfig:
        return ProcessorConfig(
            name="whitespace_normalization",
            description="Normalize whitespace and indentation",
            applies_to=["*"],  # Apply to all languages
            priority=0,
        )


class TypeSignatureProcessor(CodeProcessor):
    """Processor for handling type signatures in functional languages."""

    def process(self, code: str, language: str, **kwargs) -> str:
        """Ensure type signatures are present for Haskell functions."""
        if language != "haskell":
            return code

        # Check if function has type signature
        lines = code.split("\n")
        function_lines = []
        has_signature = False

        for line in lines:
            if "::" in line:
                has_signature = True
            if line.strip().startswith("--") or not line.strip():
                continue
            function_lines.append(line)

        # If no signature found and we have a function definition,
        # we'll let it pass but could add basic signature inference here
        return code

    def get_config(self) -> ProcessorConfig:
        return ProcessorConfig(
            name="type_signature",
            description="Handle type signatures in functional languages",
            applies_to=["haskell"],
            priority=3,
        )


class ModuleWrapperProcessor(CodeProcessor):
    """Processor for wrapping code in module structures when needed."""

    def process(self, code: str, language: str, **kwargs) -> str:
        """Wrap code in module structure if needed."""
        if language == "haskell":
            # Check if already has module declaration
            if not re.match(r"^\s*module\s+", code, re.MULTILINE):
                # For validation purposes, we might want to add a module wrapper
                # But for execution, the adapter will handle this
                pass

        return code

    def get_config(self) -> ProcessorConfig:
        return ProcessorConfig(
            name="module_wrapper",
            description="Wrap code in module structures when needed",
            applies_to=["haskell"],
            priority=8,
        )


class CommentStripProcessor(CodeProcessor):
    """Processor for stripping unwanted comments from extracted code."""

    def __init__(self):
        self.comment_patterns = {
            "python": [r"#.*$"],
            "java": [r"//.*$", r"/\*.*?\*/"],
            "javascript": [r"//.*$", r"/\*.*?\*/"],
            "c": [r"//.*$", r"/\*.*?\*/"],
            "cpp": [r"//.*$", r"/\*.*?\*/"],
            "rust": [r"//.*$", r"/\*.*?\*/"],
            "sql": [r"--.*$", r"/\*.*?\*/"],
            "haskell": [r"--.*$", r"\{-.*?-\}"],
        }

    def process(self, code: str, language: str, **kwargs) -> str:
        """Strip comments if requested."""
        strip_comments = kwargs.get("strip_comments", False)
        if not strip_comments or language not in self.comment_patterns:
            return code

        for pattern in self.comment_patterns[language]:
            if pattern.endswith("$"):
                # Line comments
                code = re.sub(pattern, "", code, flags=re.MULTILINE)
            else:
                # Block comments
                code = re.sub(pattern, "", code, flags=re.DOTALL)

        return code

    def get_config(self) -> ProcessorConfig:
        return ProcessorConfig(
            name="comment_strip",
            description="Strip comments from extracted code",
            applies_to=["*"],
            priority=2,
        )


class LanguageCodeProcessor:
    """Main code processor that applies language-specific transformations."""

    def __init__(self):
        """Initialize with all available processors."""
        self.processors: Dict[str, CodeProcessor] = {
            processor_class().get_config().name: processor_class()
            for processor_class in [
                HeaderInjectionProcessor,
                ClassWrappingProcessor,
                SemicolonCleanupProcessor,
                WhitespaceNormalizationProcessor,
                TypeSignatureProcessor,
                ModuleWrapperProcessor,
                CommentStripProcessor,
            ]
        }

        # Language-specific processor configurations
        self.language_processors: Dict[str, List[str]] = {
            "python": ["whitespace_normalization", "comment_strip"],
            "java": ["class_wrapping", "whitespace_normalization", "comment_strip"],
            "javascript": ["whitespace_normalization", "comment_strip"],
            "c": ["header_injection", "whitespace_normalization", "comment_strip"],
            "cpp": ["header_injection", "whitespace_normalization", "comment_strip"],
            "rust": ["whitespace_normalization", "comment_strip"],
            "haskell": [
                "type_signature",
                "module_wrapper",
                "whitespace_normalization",
                "comment_strip",
            ],
            "sql": ["semicolon_cleanup", "whitespace_normalization", "comment_strip"],
            "go": ["whitespace_normalization", "comment_strip"],
            "kotlin": ["class_wrapping", "whitespace_normalization", "comment_strip"],
            "php": ["whitespace_normalization", "comment_strip"],
            "ruby": ["whitespace_normalization", "comment_strip"],
            "typescript": ["whitespace_normalization", "comment_strip"],
            "bash": ["whitespace_normalization"],
            "ocaml": ["whitespace_normalization", "comment_strip"],
        }

    def process_code(self, code: str, language: str, **kwargs) -> str:
        """Process code using language-specific processors.

        Args:
            code: The extracted code to process
            language: The programming language
            **kwargs: Additional options for processors

        Returns:
            Processed code
        """
        if not code or language not in self.language_processors:
            # Apply basic whitespace normalization for unknown languages
            return self.processors["whitespace_normalization"].process(
                code, language, **kwargs
            )

        processor_names = self.language_processors[language]

        # Sort processors by priority (higher priority first)
        sorted_processors = sorted(
            [
                (name, self.processors[name])
                for name in processor_names
                if name in self.processors
            ],
            key=lambda x: x[1].get_config().priority,
            reverse=True,
        )

        # Apply processors in order
        processed_code = code
        for name, processor in sorted_processors:
            processed_code = processor.process(processed_code, language, **kwargs)

        return processed_code

    def get_available_processors(self, language: str) -> List[str]:
        """Get list of available processors for a language."""
        return self.language_processors.get(language, ["whitespace_normalization"])

    def add_processor(self, processor: CodeProcessor) -> None:
        """Add a custom processor."""
        config = processor.get_config()
        self.processors[config.name] = processor

        # Add to applicable languages
        for lang in config.applies_to:
            if lang == "*":
                # Add to all languages
                for lang_list in self.language_processors.values():
                    if config.name not in lang_list:
                        lang_list.append(config.name)
            else:
                if lang not in self.language_processors:
                    self.language_processors[lang] = []
                if config.name not in self.language_processors[lang]:
                    self.language_processors[lang].append(config.name)

    def process_with_debug(self, code: str, language: str, **kwargs) -> Dict[str, Any]:
        """Process code with debug information.

        Returns:
            Dictionary with processed code and debug info
        """
        if not code or language not in self.language_processors:
            processed = self.processors["whitespace_normalization"].process(
                code, language, **kwargs
            )
            return {
                "original_code": code,
                "processed_code": processed,
                "processors_applied": ["whitespace_normalization"],
                "language": language,
            }

        processor_names = self.language_processors[language]

        # Sort processors by priority
        sorted_processors = sorted(
            [
                (name, self.processors[name])
                for name in processor_names
                if name in self.processors
            ],
            key=lambda x: x[1].get_config().priority,
            reverse=True,
        )

        # Apply processors and track changes
        processed_code = code
        processors_applied = []

        for name, processor in sorted_processors:
            old_code = processed_code
            processed_code = processor.process(processed_code, language, **kwargs)
            if processed_code != old_code:
                processors_applied.append(name)

        return {
            "original_code": code,
            "processed_code": processed_code,
            "processors_applied": processors_applied,
            "language": language,
            "total_processors_available": len(sorted_processors),
        }


# Convenience functions for common use cases
def process_extracted_code(code: str, language: str, **kwargs) -> str:
    """Convenience function to process extracted code."""
    processor = LanguageCodeProcessor()
    return processor.process_code(code, language, **kwargs)


def get_language_processors(language: str) -> List[str]:
    """Get available processors for a language."""
    processor = LanguageCodeProcessor()
    return processor.get_available_processors(language)


def create_custom_processor(
    name: str,
    process_func: Callable[[str, str], str],
    applies_to: List[str],
    priority: int = 0,
) -> CodeProcessor:
    """Create a custom processor from a function.

    Args:
        name: Processor name
        process_func: Function that takes (code, language) and returns processed code
        applies_to: List of languages this processor applies to
        priority: Processor priority

    Returns:
        Custom processor instance
    """

    class CustomProcessor(CodeProcessor):
        def process(self, code: str, language: str, **kwargs) -> str:
            return process_func(code, language)

        def get_config(self) -> ProcessorConfig:
            return ProcessorConfig(
                name=name,
                description=f"Custom processor: {name}",
                applies_to=applies_to,
                priority=priority,
            )

    return CustomProcessor()
