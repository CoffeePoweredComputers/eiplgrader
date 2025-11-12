"""Simple base classes for language adapters."""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Optional
from tree_sitter import Language, Parser, Query, QueryCursor
import re


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


    def normalize_code(self, code: str) -> str:
        """Normalize code by removing comments and standardizing format."""
        lang = self._get_lang()
        parser = Parser(lang)

        error_capture = Query(
            lang,
            '''(ERROR) @error
            (MISSING) @error'''
        )
        comment_capture = Query(
            lang,
            '''(comment) @comment'''
        )

        source = bytes(code, "utf8")
        tree = parser.parse(source)

        errors = QueryCursor(error_capture).captures(tree.root_node)
        if "error" in errors and len(errors["error"]) > 0:
            raise SyntaxError("LLM generated code has syntax errors, unable to parse")

        # Create a list of the character index ranges of comments, in order
        captures = QueryCursor(comment_capture).captures(tree.root_node)
        ranges = [(comment.start_byte, comment.end_byte) for comment in captures["comment"]]
        ranges.sort(key=lambda r: r[0])

        # Grab each segment of text which is not a comment and stitch them back together
        sections = []
        current_pos = 0
        for start, end in ranges:
            sections.append(source[current_pos:start])
            current_pos = end
        sections.append(source[current_pos:])
        source = b''.join(sections)

        # Reduce multiple blank lines to one blank line
        text = source.decode("utf-8")
        text = re.sub(r"\n\s*\n", "\n", text)
        return text


    @abstractmethod
    def _get_lang(self) -> Language:
        """Gets the tree-sitter language object for an adapter"""
        raise NotImplementedError
