"""
String building utilities for language executors.

This module provides infrastructure for structured string building with proper
indentation support, template utilities, and common string operations used
across different language executors.
"""

import textwrap
from typing import List, Union, Optional


class CodeBuilder:
    """
    A structured string builder with automatic indentation support.

    Supports adding lines with automatic indentation, increasing/decreasing
    indent levels, and building the final string output. Can be used as a
    context manager for indent blocks.

    Example:
        builder = CodeBuilder()
        builder.add_line("public class Test {")
        with builder.indent():
            builder.add_line("public static void main(String[] args) {")
            with builder.indent():
                builder.add_line("System.out.println(\"Hello, World!\");")
            builder.add_line("}")
        builder.add_line("}")
        result = builder.build()
    """

    def __init__(self, indent_size: int = 4):
        """
        Initialize CodeBuilder with specified indentation size.

        Args:
            indent_size: Number of spaces per indentation level (default: 4)
        """
        self._lines: List[str] = []
        self._indent_level: int = 0
        self._indent_size: int = indent_size

    def add_line(self, line: str = "") -> None:
        """
        Add a line with current indentation level.

        Args:
            line: The line to add (default: empty line)
        """
        if line.strip():  # Non-empty line gets indentation
            indented_line = " " * (self._indent_level * self._indent_size) + line
            self._lines.append(indented_line)
        else:  # Empty line stays empty
            self._lines.append("")

    def add_lines(self, lines: Union[str, List[str]]) -> None:
        """
        Add multiple lines, each with current indentation level.

        Args:
            lines: String with newlines or list of strings
        """
        if isinstance(lines, str):
            lines = lines.split("\n")

        for line in lines:
            self.add_line(line)

    def add_raw(self, text: str) -> None:
        """
        Add text without any indentation processing.

        Args:
            text: Raw text to add (may contain newlines)
        """
        self._lines.extend(text.split("\n"))

    def increase_indent(self) -> None:
        """Increase the current indentation level by one."""
        self._indent_level += 1

    def decrease_indent(self) -> None:
        """Decrease the current indentation level by one (minimum 0)."""
        self._indent_level = max(0, self._indent_level - 1)

    def indent(self):
        """
        Context manager for temporary indentation increase.

        Returns:
            Context manager that increases indent on enter, decreases on exit
        """
        return _IndentContext(self)

    def build(self) -> str:
        """
        Build and return the final string.

        Returns:
            The complete string with all lines joined by newlines
        """
        return "\n".join(self._lines)

    def clear(self) -> None:
        """Clear all accumulated lines and reset to initial state."""
        self._lines.clear()
        self._indent_level = 0

    def get_indent_level(self) -> int:
        """Get the current indentation level."""
        return self._indent_level

    def __len__(self) -> int:
        """Return the number of lines currently stored."""
        return len(self._lines)

    def __str__(self) -> str:
        """Return the built string."""
        return self.build()


class _IndentContext:
    """Context manager for CodeBuilder indentation."""

    def __init__(self, builder: CodeBuilder):
        self._builder = builder

    def __enter__(self):
        self._builder.increase_indent()
        return self._builder

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._builder.decrease_indent()


def indent_block(text: str, spaces: int = 4) -> str:
    """
    Indent a block of text by the specified number of spaces.

    Args:
        text: The text block to indent
        spaces: Number of spaces to indent (default: 4)

    Returns:
        The indented text block

    Example:
        >>> code = "def foo():\\n    return 42"
        >>> indent_block(code, 2)
        "  def foo():\\n      return 42"
    """
    if not text.strip():
        return text

    indent_str = " " * spaces
    lines = text.split("\n")
    indented_lines = []

    for line in lines:
        if line.strip():  # Non-empty line
            indented_lines.append(indent_str + line)
        else:  # Empty line
            indented_lines.append("")

    return "\n".join(indented_lines)


def dedent_template(template: str) -> str:
    """
    Wrapper around textwrap.dedent for consistent template processing.

    This function normalizes indentation in multi-line strings, making them
    easier to work with when building code templates.

    Args:
        template: The template string to dedent

    Returns:
        The dedented template string

    Example:
        >>> template = '''
        ...     def foo():
        ...         return 42
        ...     '''
        >>> dedent_template(template).strip()
        "def foo():\\n    return 42"
    """
    return textwrap.dedent(template)


def escape_string_literal(value: str, quote_char: str = '"') -> str:
    """
    Escape a string value for use as a string literal in generated code.

    Args:
        value: The string value to escape
        quote_char: The quote character to use (default: ")

    Returns:
        The escaped string literal with quotes

    Example:
        >>> escape_string_literal('Hello "World"')
        '"Hello \\"World\\""'
    """
    # Basic escaping for common cases
    escaped = value.replace("\\", "\\\\")  # Escape backslashes first
    escaped = escaped.replace(quote_char, f"\\{quote_char}")  # Escape quotes
    escaped = escaped.replace("\n", "\\n")  # Escape newlines
    escaped = escaped.replace("\t", "\\t")  # Escape tabs
    escaped = escaped.replace("\r", "\\r")  # Escape carriage returns

    return f"{quote_char}{escaped}{quote_char}"


def build_parameter_list(
    param_names: List[str],
    param_types: Optional[List[str]] = None,
    separator: str = ", ",
) -> str:
    """
    Build a parameter list string, optionally with types.

    Args:
        param_names: List of parameter names
        param_types: Optional list of parameter types (must match length of names)
        separator: String to separate parameters (default: ", ")

    Returns:
        The formatted parameter list string

    Example:
        >>> build_parameter_list(["x", "y"])
        "x, y"
        >>> build_parameter_list(["x", "y"], ["int", "string"])
        "int x, string y"
    """
    if not param_names:
        return ""

    if param_types:
        if len(param_names) != len(param_types):
            raise ValueError("param_names and param_types must have same length")
        pairs = [f"{ptype} {pname}" for ptype, pname in zip(param_types, param_names)]
        return separator.join(pairs)
    else:
        return separator.join(param_names)


def decode_escape_sequences(value: str) -> str:
    """
    Decode escape sequences in a string value.

    This function converts literal escape sequences like '\\n', '\\t', '\\r'
    to their actual character equivalents. This is useful for processing
    test case parameters that contain escape sequences in JSON format.

    Args:
        value: The string value that may contain escape sequences

    Returns:
        The string with escape sequences decoded

    Example:
        >>> decode_escape_sequences("hello\\nworld\\t")
        "hello\nworld\t"
        >>> decode_escape_sequences("test\\r\\n123")
        "test\r\n123"
    """
    # Replace common escape sequences
    decoded = value.replace("\\n", "\n")  # Newline
    decoded = decoded.replace("\\t", "\t")  # Tab
    decoded = decoded.replace("\\r", "\r")  # Carriage return
    decoded = decoded.replace("\\\\", "\\")  # Backslash (must be last)

    return decoded


def process_test_parameters(parameters: dict) -> dict:
    """
    Process test case parameters, decoding escape sequences in string values.

    Args:
        parameters: Dictionary of parameter names to values

    Returns:
        Dictionary with string values having escape sequences decoded
    """
    processed = {}
    for key, value in parameters.items():
        if isinstance(value, str):
            processed[key] = decode_escape_sequences(value)
        else:
            processed[key] = value
    return processed
