"""Unified code extraction system for all language adapters."""

import re
from typing import List, Optional
from .spec import LanguageSpec


class CodeExtractor:
    """Unified code extraction with language-specific patterns"""
    
    def extract(self, llm_response: str, spec: LanguageSpec) -> List[str]:
        """Extract code using multiple fallback methods"""
        
        # Method 1: Markdown code blocks
        code_blocks = self._extract_markdown_blocks(llm_response, spec)
        if code_blocks:
            return self._process_extracted_code(code_blocks, spec)
        
        # Method 2: Split by markdown delimiters
        delimiter_blocks = self._extract_by_delimiter(llm_response, spec)
        if delimiter_blocks:
            return self._process_extracted_code(delimiter_blocks, spec)
        
        # Method 3: Language-specific function patterns
        pattern_blocks = self._extract_by_patterns(llm_response, spec)
        if pattern_blocks:
            return self._process_extracted_code(pattern_blocks, spec)
        
        # Method 4: Fallback
        return self._fallback_extraction(llm_response, spec)
    
    def _extract_markdown_blocks(self, response: str, spec: LanguageSpec) -> List[str]:
        """Method 1: Extract ```language blocks"""
        pattern = rf"```{spec.code_block_tag}\n(.*?)```"
        matches = re.findall(pattern, response, re.DOTALL)
        return [match.strip() for match in matches]
    
    def _extract_by_delimiter(self, response: str, spec: LanguageSpec) -> List[str]:
        """Method 2: Split by markdown delimiters"""
        if f"```{spec.code_block_tag}" not in response:
            return []
        
        functions = []
        parts = response.split(f"```{spec.code_block_tag}")
        for part in parts[1:]:  # Skip first empty part
            code = part.split("```")[0].strip()
            if code:
                functions.append(code)
        return functions
    
    def _extract_by_patterns(self, response: str, spec: LanguageSpec) -> List[str]:
        """Method 3: Language-specific function patterns"""
        extracted = []
        for pattern in spec.function_patterns:
            matches = re.findall(pattern, response, re.DOTALL)
            if matches:
                # Handle tuple results from complex patterns
                for match in matches:
                    if isinstance(match, tuple):
                        # Find first non-empty group
                        for group in match:
                            if group and group.strip():
                                extracted.append(group.strip())
                                break
                    else:
                        extracted.append(match.strip())
        return extracted
    
    def _fallback_extraction(self, response: str, spec: LanguageSpec) -> List[str]:
        """Method 4: Fallback strategies"""
        if response.strip():
            return self._process_extracted_code([response.strip()], spec)
        return []
    
    def _process_extracted_code(self, matches: List[str], spec: LanguageSpec) -> List[str]:
        """Post-process extracted code"""
        processed = []
        for match in matches:
            code = match.strip()
            
            # Auto-inject headers if needed and missing
            if (spec.header_requirements and 
                spec.header_requirements not in code and
                not any(header in code for header in ["#include", "import", "using"])):
                code = spec.header_requirements + "\n\n" + code
            
            processed.append(code)
        return processed


class CodeProcessor:
    """Language-specific code post-processing"""
    
    @staticmethod
    def process(code: str, spec: LanguageSpec) -> str:
        """Apply language-specific processing"""
        # Header injection
        if (spec.header_requirements and 
            spec.header_requirements not in code and
            spec.name in ["c", "cpp"]):
            code = CodeProcessor._inject_headers(code, spec)
        
        # Class wrapping for Java
        if spec.name == "java" and "class" not in code.lower():
            code = CodeProcessor._wrap_in_class(code, spec)
        
        # SQL semicolon cleanup
        if spec.name == "sql":
            code = CodeProcessor._clean_semicolons(code, spec)
        
        # Normalize whitespace
        code = CodeProcessor._normalize_whitespace(code)
        
        return code
    
    @staticmethod
    def _inject_headers(code: str, spec: LanguageSpec) -> str:
        """Inject required headers/imports if missing"""
        if spec.header_requirements and spec.header_requirements not in code:
            return spec.header_requirements + "\n\n" + code
        return code
    
    @staticmethod
    def _wrap_in_class(code: str, spec: LanguageSpec) -> str:
        """Wrap standalone methods in classes (Java)"""
        if "public class" not in code:
            return f"public class Solution {{\n    {code}\n}}"
        return code
    
    @staticmethod
    def _clean_semicolons(code: str, spec: LanguageSpec) -> str:
        """Remove trailing semicolons (SQL)"""
        return code.rstrip(";").strip()
    
    @staticmethod
    def _normalize_whitespace(code: str) -> str:
        """Normalize indentation and whitespace"""
        lines = code.split("\n")
        # Remove empty lines at start/end
        while lines and not lines[0].strip():
            lines.pop(0)
        while lines and not lines[-1].strip():
            lines.pop()
        return "\n".join(lines)