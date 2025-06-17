"""Error message processing and standardization for validation results."""

import re
from typing import Dict, Optional, List, Tuple
from pathlib import Path


class ErrorMessageProcessor:
    """Standardize error messages across different validators and languages."""
    
    # Common patterns to remove from error messages
    TEMP_FILE_PATTERNS = [
        r'/tmp/[a-zA-Z0-9_\-\.]+',
        r'/var/folders/[^:]+',
        r'C:\\Users\\[^\\]+\\AppData\\Local\\Temp\\[^:]+',
        r'tmp[a-zA-Z0-9_\-\.]+\.(py|js|c|cpp|java|rs|go|rb|php|kt|ml|hs|sh|sql)',
    ]
    
    # Language-specific error patterns and replacements
    LANGUAGE_ERROR_PATTERNS: Dict[str, List[Tuple[str, str]]] = {
        'python': [
            (r'File "<string>", ', ''),
            (r'File "[^"]+", ', ''),
            (r'^\s*\^+\s*$', ''),  # Remove caret lines
        ],
        'javascript': [
            (r'file:///[^:]+:', ''),
            (r'at [^(]+\([^)]+\)', ''),  # Remove stack traces
        ],
        'c': [
            (r'([^:]+\.(c|h)):', 'code:'),
            (r'In function \'[^\']+\':', ''),
        ],
        'java': [
            (r'([^:]+\.java):', 'Solution.java:'),
            (r'symbol:\s+', ''),
            (r'location:\s+', ''),
        ],
    }
    
    def __init__(self):
        """Initialize the error processor."""
        self.max_error_lines = 5  # Maximum lines to show in error message
        self.max_error_length = 500  # Maximum total error length
    
    def process_error(self, error: str, language: str, temp_file: Optional[str] = None) -> str:
        """Process and clean up an error message.
        
        Args:
            error: Raw error message
            language: Language name for language-specific processing
            temp_file: Temporary file path to remove from error
            
        Returns:
            Cleaned error message
        """
        if not error:
            return "Unknown error"
        
        # Step 1: Remove temporary file paths
        cleaned = self._remove_temp_paths(error, temp_file)
        
        # Step 2: Apply language-specific patterns
        cleaned = self._apply_language_patterns(cleaned, language)
        
        # Step 3: Clean up whitespace and empty lines
        cleaned = self._clean_whitespace(cleaned)
        
        # Step 4: Limit error size
        cleaned = self._limit_error_size(cleaned)
        
        # Step 5: Format for readability
        cleaned = self._format_error(cleaned, language)
        
        return cleaned
    
    def process_compiler_error(self, stderr: str, language: str) -> str:
        """Process compiler error messages.
        
        Args:
            stderr: Standard error output from compiler
            language: Language name
            
        Returns:
            Processed error message
        """
        # Extract relevant errors only
        relevant_errors = self._extract_relevant_errors(stderr, language)
        
        # Process each error
        processed_errors = []
        for error in relevant_errors:
            processed = self.process_error(error, language)
            if processed and processed != "Unknown error":
                processed_errors.append(processed)
        
        # Join errors with clear separation
        if processed_errors:
            return "\n".join(processed_errors[:3])  # Limit to first 3 errors
        else:
            return self.process_error(stderr, language)
    
    def process_parser_error(self, error: Exception, language: str) -> str:
        """Process parser exception messages.
        
        Args:
            error: Parser exception
            language: Language name
            
        Returns:
            Formatted error message
        """
        error_str = str(error)
        
        # Special handling for Python SyntaxError
        if language == "python" and hasattr(error, 'msg') and hasattr(error, 'lineno'):
            return f"Syntax error at line {error.lineno}: {error.msg}"
        
        return self.process_error(error_str, language)
    
    def _remove_temp_paths(self, error: str, temp_file: Optional[str] = None) -> str:
        """Remove temporary file paths from error messages."""
        cleaned = error
        
        # Remove specific temp file if provided
        if temp_file:
            temp_path = Path(temp_file)
            cleaned = cleaned.replace(str(temp_path), "code")
            cleaned = cleaned.replace(temp_path.name, "code")
        
        # Remove common temp path patterns
        for pattern in self.TEMP_FILE_PATTERNS:
            cleaned = re.sub(pattern, "code", cleaned)
        
        return cleaned
    
    def _apply_language_patterns(self, error: str, language: str) -> str:
        """Apply language-specific error cleaning patterns."""
        if language in self.LANGUAGE_ERROR_PATTERNS:
            for pattern, replacement in self.LANGUAGE_ERROR_PATTERNS[language]:
                error = re.sub(pattern, replacement, error, flags=re.MULTILINE)
        return error
    
    def _clean_whitespace(self, error: str) -> str:
        """Clean up whitespace and empty lines."""
        # Split into lines
        lines = error.split('\n')
        
        # Remove empty lines and excessive whitespace
        cleaned_lines = []
        for line in lines:
            line = line.rstrip()
            if line:  # Skip empty lines
                cleaned_lines.append(line)
        
        return '\n'.join(cleaned_lines)
    
    def _limit_error_size(self, error: str) -> str:
        """Limit error message to reasonable size."""
        lines = error.split('\n')
        
        # Limit number of lines
        if len(lines) > self.max_error_lines:
            lines = lines[:self.max_error_lines]
            lines.append("...")
        
        result = '\n'.join(lines)
        
        # Limit total length
        if len(result) > self.max_error_length:
            result = result[:self.max_error_length] + "..."
        
        return result
    
    def _format_error(self, error: str, language: str) -> str:
        """Format error for better readability."""
        # Add language prefix if not already present
        if not error.lower().startswith(f"{language} "):
            error = f"{language.capitalize()} error: {error}"
        
        return error
    
    def _extract_relevant_errors(self, stderr: str, language: str) -> List[str]:
        """Extract only relevant error messages from compiler output."""
        lines = stderr.split('\n')
        errors = []
        current_error = []
        
        # Language-specific error detection patterns
        error_start_patterns = {
            'c': r'.*:\d+:\d+: (error|warning):',
            'cpp': r'.*:\d+:\d+: (error|warning):',
            'java': r'.*\.java:\d+: error:',
            'rust': r'error(\[E\d+\])?:',
            'go': r'.*\.go:\d+:\d+:',
        }
        
        pattern = error_start_patterns.get(language, r'error:')
        
        for line in lines:
            if re.match(pattern, line):
                # Start of new error
                if current_error:
                    errors.append('\n'.join(current_error))
                current_error = [line]
            elif current_error:
                # Continuation of current error
                current_error.append(line)
        
        # Don't forget the last error
        if current_error:
            errors.append('\n'.join(current_error))
        
        return errors


# Singleton instance for convenience
error_processor = ErrorMessageProcessor()


def clean_validation_error(error: str, language: str) -> str:
    """Convenience function to clean validation errors.
    
    Args:
        error: Raw error message
        language: Language name
        
    Returns:
        Cleaned error message
    """
    return error_processor.process_error(error, language)