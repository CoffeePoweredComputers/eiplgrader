"""Validation strategies for language adapters."""

import subprocess
import tempfile
import ast
import re
import os
from abc import ABC, abstractmethod
from typing import Optional, Tuple, List, Dict, Any
from dataclasses import dataclass

from .spec import LanguageSpec
from .error_processor import error_processor


class ValidationStrategy(ABC):
    """Abstract base class for validation strategies."""
    
    @abstractmethod
    def validate(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate code and return (is_valid, error_message)."""
        pass


@dataclass
class ValidationResult:
    """Result of code validation."""
    is_valid: bool
    error_message: Optional[str] = None
    warnings: List[str] = None
    metadata: Dict[str, Any] = None
    
    def __post_init__(self):
        if self.warnings is None:
            self.warnings = []
        if self.metadata is None:
            self.metadata = {}


class CompilerValidator(ValidationStrategy):
    """Validation using external compiler/interpreter."""
    
    def __init__(self, spec: LanguageSpec):
        self.spec = spec
        self.validation_command = spec.get_validation_command()
        
    def validate(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate code using compiler/interpreter."""
        if not self.validation_command:
            # Fallback to compile_command if no validation_command
            if self.spec.compile_command:
                return self._validate_with_compiler(code)
            else:
                return True, None  # No validation available
        
        return self._validate_with_command(code, self.validation_command)
    
    def _validate_with_command(self, code: str, command: List[str]) -> Tuple[bool, Optional[str]]:
        """Validate using a specific command."""
        try:
            # Prepare code with necessary headers/imports
            prepared_code = self._prepare_code_for_validation(code)
            
            # Write to temporary file
            with tempfile.NamedTemporaryFile(
                mode="w", 
                suffix=self.spec.file_extensions[0], 
                delete=False
            ) as f:
                f.write(prepared_code)
                temp_path = f.name
            
            try:
                # Replace {file} placeholder in command if present
                final_command = [
                    temp_path if arg == "{file}" else arg 
                    for arg in command
                ]
                
                # If no {file} placeholder, append file path
                if "{file}" not in command:
                    final_command.append(temp_path)
                
                # Run validation command
                result = subprocess.run(
                    final_command,
                    capture_output=True,
                    text=True,
                    timeout=30
                )
                
                if result.returncode == 0:
                    return True, None
                else:
                    error_msg = error_processor.process_compiler_error(
                        result.stderr or result.stdout, 
                        self.spec.name
                    )
                    return False, error_msg
                    
            finally:
                # Clean up temporary file
                if os.path.exists(temp_path):
                    os.unlink(temp_path)
                    
        except subprocess.TimeoutExpired:
            return False, "Validation timeout"
        except FileNotFoundError:
            return False, f"Validation command not found: {command[0]}"
        except Exception as e:
            return False, f"Validation error: {str(e)}"
    
    def _validate_with_compiler(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate using compile command with syntax-only flag."""
        compile_cmd = self.spec.compile_command[:]
        
        # Add syntax-only flags for common compilers
        if compile_cmd[0] in ["gcc", "g++", "clang", "clang++"]:
            compile_cmd.append("-fsyntax-only")
        elif compile_cmd[0] == "javac":
            compile_cmd.extend(["-Xlint", "-Xdiags:verbose"])
        
        return self._validate_with_command(code, compile_cmd)
    
    def _prepare_code_for_validation(self, code: str) -> str:
        """Prepare code for validation by adding necessary headers/imports."""
        prepared = code
        
        # Add required imports if missing
        if hasattr(self.spec, 'header_requirements') and self.spec.header_requirements:
            for import_stmt in self.spec.header_requirements.required_imports:
                if import_stmt not in code:
                    prepared = import_stmt + "\n" + prepared
        
        # For compiled languages, add a dummy main if needed
        if getattr(self.spec, 'requires_main_wrapper', False) and "main" not in code.lower():
            if self.spec.name == "c":
                prepared += "\n\nint main() { return 0; }"
            elif self.spec.name == "cpp":
                prepared += "\n\nint main() { return 0; }"
            elif self.spec.name == "java":
                prepared += "\n\npublic class Test { public static void main(String[] args) {} }"
        
        return prepared
    


class ParserValidator(ValidationStrategy):
    """Validation using language-specific parsers."""
    
    def __init__(self, spec: LanguageSpec):
        self.spec = spec
    
    def validate(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate code using built-in parsers."""
        if self.spec.name == "python":
            return self._validate_python(code)
        elif self.spec.name in ["javascript", "typescript"]:
            return self._validate_javascript(code)
        else:
            # Generic validation using regex patterns
            return self._validate_generic(code)
    
    def _validate_python(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Python code using ast module."""
        try:
            ast.parse(code)
            return True, None
        except SyntaxError as e:
            return False, error_processor.process_parser_error(e, "python")
        except Exception as e:
            return False, error_processor.process_error(str(e), "python")
    
    def _validate_javascript(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate JavaScript code using node --check if available."""
        # Try node validation first
        try:
            with tempfile.NamedTemporaryFile(mode="w", suffix=".js", delete=False) as f:
                f.write(code)
                temp_path = f.name
            
            try:
                result = subprocess.run(
                    ["node", "--check", temp_path],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                
                if result.returncode == 0:
                    return True, None
                else:
                    error_msg = error_processor.process_error(
                        result.stderr.strip(), 
                        "javascript",
                        temp_path
                    )
                    return False, error_msg
                    
            finally:
                if os.path.exists(temp_path):
                    os.unlink(temp_path)
                    
        except (FileNotFoundError, subprocess.TimeoutExpired):
            # Fallback to basic syntax checks
            issues = []
            
            # Check for unmatched braces
            if code.count('{') != code.count('}'):
                issues.append("Unmatched braces")
            
            # Check for unmatched parentheses
            if code.count('(') != code.count(')'):
                issues.append("Unmatched parentheses")
            
            # Check for basic function syntax
            if 'function' in code:
                if not re.search(r'function\s+\w+\s*\([^)]*\)\s*{', code):
                    issues.append("Invalid function syntax")
            
            if issues:
                return False, "; ".join(issues)
            return True, None
    
    def _validate_generic(self, code: str) -> Tuple[bool, Optional[str]]:
        """Generic validation using syntax conventions."""
        issues = []
        
        # Check for unmatched delimiters if conventions are available
        if hasattr(self.spec, 'syntax_conventions') and self.spec.syntax_conventions:
            conventions = self.spec.syntax_conventions
            if conventions.block_start and conventions.block_end:
                if code.count(conventions.block_start) != code.count(conventions.block_end):
                    issues.append(f"Unmatched {conventions.block_start}/{conventions.block_end}")
        
        # Check for basic function patterns
        if hasattr(self.spec, 'function_patterns') and self.spec.function_patterns:
            pattern = self.spec.function_patterns.definition_regex
            if not re.search(pattern, code):
                issues.append("No valid function definitions found")
        
        if issues:
            return False, "; ".join(issues)
        return True, None


class CustomValidator(ValidationStrategy):
    """Custom validation strategy for language-specific rules."""
    
    def __init__(self, spec: LanguageSpec, validation_func=None):
        self.spec = spec
        self.validation_func = validation_func
    
    def validate(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate using custom validation function or language-specific method."""
        if self.validation_func:
            return self.validation_func(code)
        
        # Language-specific validation
        if self.spec.name == "sql":
            return self._validate_sql(code)
        elif self.spec.name == "bash":
            return self._validate_bash(code)
        
        return True, None  # No custom validation defined
    
    def _validate_sql(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate SQL syntax using sqlite3."""
        try:
            import sqlite3
            
            # Create in-memory database for validation
            conn = sqlite3.connect(":memory:")
            cursor = conn.cursor()
            
            # Clean up query - ensure it ends with semicolon for validation
            query = code.strip()
            if not query.endswith(";"):
                query += ";"
            
            try:
                # Use EXPLAIN QUERY PLAN to validate without executing
                cursor.execute(f"EXPLAIN QUERY PLAN {query}")
                conn.close()
                return True, None
                
            except sqlite3.Error as e:
                error_msg = str(e)
                # If error is about missing tables/columns, that's OK - syntax is valid
                if any(msg in error_msg.lower() for msg in ["no such table", "no such column"]):
                    return True, None
                return False, f"SQL syntax error: {error_msg}"
                
        except ImportError:
            # sqlite3 not available, do basic validation
            keywords = ["SELECT", "INSERT", "UPDATE", "DELETE", "CREATE", "DROP", "ALTER"]
            if not any(keyword in code.upper() for keyword in keywords):
                return False, "No valid SQL statement found"
            return True, None
        except Exception as e:
            return False, f"SQL validation error: {str(e)}"
    
    def _validate_bash(self, code: str) -> Tuple[bool, Optional[str]]:
        """Validate Bash syntax using bash -n."""
        try:
            with tempfile.NamedTemporaryFile(mode="w", suffix=".sh", delete=False) as f:
                f.write(code)
                temp_path = f.name
            
            try:
                result = subprocess.run(
                    ["bash", "-n", temp_path],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                
                if result.returncode == 0:
                    return True, None
                else:
                    error_msg = error_processor.process_error(
                        result.stderr.strip(),
                        "bash",
                        temp_path
                    )
                    return False, error_msg
                    
            finally:
                if os.path.exists(temp_path):
                    os.unlink(temp_path)
                    
        except (FileNotFoundError, subprocess.TimeoutExpired):
            # Fallback to basic checks
            issues = []
            
            # Check for common syntax errors
            if code.count("if") > code.count("fi"):
                issues.append("Unclosed if statement")
            if code.count("do") > code.count("done"):
                issues.append("Unclosed do loop")
            if code.count("case") > code.count("esac"):
                issues.append("Unclosed case statement")
            
            if issues:
                return False, "; ".join(issues)
            return True, None


class NoValidator(ValidationStrategy):
    """No-op validator that always passes."""
    
    def validate(self, code: str) -> Tuple[bool, Optional[str]]:
        """Always return valid."""
        return True, None


class CompositeValidator(ValidationStrategy):
    """Composite validator that runs multiple validation strategies."""
    
    def __init__(self, validators: List[ValidationStrategy], stop_on_first_error: bool = True):
        self.validators = validators
        self.stop_on_first_error = stop_on_first_error
    
    def validate(self, code: str) -> Tuple[bool, Optional[str]]:
        """Run all validators and return combined result."""
        errors = []
        
        for validator in self.validators:
            is_valid, error = validator.validate(code)
            if not is_valid:
                errors.append(error or "Unknown validation error")
                if self.stop_on_first_error:
                    return False, errors[0]
        
        if errors:
            return False, "; ".join(errors)
        return True, None


def create_validator(spec: LanguageSpec, custom_validation_func=None) -> ValidationStrategy:
    """Factory function to create appropriate validator based on spec."""
    strategy = spec.validation_strategy.lower()
    
    if strategy == "compiler":
        return CompilerValidator(spec)
    elif strategy == "parser":
        return ParserValidator(spec)
    elif strategy == "custom":
        return CustomValidator(spec, custom_validation_func)
    elif strategy == "none":
        return NoValidator()
    else:
        # Default to parser validation
        return ParserValidator(spec)


def create_composite_validator(
    spec: LanguageSpec, 
    strategies: List[str], 
    custom_validation_func=None
) -> CompositeValidator:
    """Create a composite validator with multiple strategies."""
    validators = []
    
    for strategy in strategies:
        if strategy == "compiler":
            validators.append(CompilerValidator(spec))
        elif strategy == "parser":
            validators.append(ParserValidator(spec))
        elif strategy == "custom":
            validators.append(CustomValidator(spec, custom_validation_func))
        elif strategy != "none":
            # Default to parser for unknown strategies
            validators.append(ParserValidator(spec))
    
    return CompositeValidator(validators)
