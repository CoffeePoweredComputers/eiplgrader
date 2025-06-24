#!/usr/bin/env python3
"""Validate Jekyll/Liquid syntax in markdown files - Version 2 with better code block handling."""

import re
import sys
from pathlib import Path
from typing import List, Tuple, Dict

class LiquidSyntaxValidator:
    def __init__(self):
        self.errors: List[Dict[str, any]] = []
        
    def is_in_code_block(self, lines: List[str], line_num: int) -> bool:
        """Check if a line is inside a code block."""
        in_code_block = False
        code_fence_pattern = re.compile(r'^```')
        
        for i in range(line_num):
            if code_fence_pattern.match(lines[i]):
                in_code_block = not in_code_block
                
        return in_code_block
        
    def validate_file(self, filepath: Path) -> List[Dict[str, any]]:
        """Validate a single markdown file for Liquid syntax errors."""
        file_errors = []
        
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                content = f.read()
                lines = content.split('\n')
                
            # Track if we're in a code block
            in_code_block = False
            
            for line_num, line in enumerate(lines, 1):
                # Check for code fence
                if line.strip().startswith('```'):
                    in_code_block = not in_code_block
                    continue
                
                # Skip validation inside code blocks
                if in_code_block:
                    continue
                
                # Also skip inline code (backticks)
                # Remove inline code before checking
                cleaned_line = re.sub(r'`[^`]+`', '', line)
                
                # Find all {{ occurrences
                var_opens = [(m.start(), m.end()) for m in re.finditer(r'\{\{', cleaned_line)]
                var_closes = [(m.start(), m.end()) for m in re.finditer(r'\}\}', cleaned_line)]
                
                # Check if opens match closes
                if len(var_opens) != len(var_closes):
                    file_errors.append({
                        'file': str(filepath),
                        'line': line_num,
                        'error': 'Mismatched {{ }} tags',
                        'content': line.strip()
                    })
                
                # Check for malformed variable syntax
                # Look for patterns like {{ {something (but not in Python f-strings)
                # Skip if line contains python code indicators
                if not any(indicator in line for indicator in ['f"', "f'", 'return f', 'print(f']):
                    malformed_pattern = r'\{\{\s*\{[^{]'
                    if re.search(malformed_pattern, cleaned_line):
                        file_errors.append({
                            'file': str(filepath),
                            'line': line_num,
                            'error': 'Malformed variable syntax (extra {)',
                            'content': line.strip()
                        })
                
                # Check for Python-style formatting within actual Liquid tags
                # This should only flag actual Liquid syntax, not Python code
                if '{{' in cleaned_line and '}}' in cleaned_line:
                    # Extract content between {{ and }}
                    liquid_content_pattern = r'\{\{([^}]+)\}\}'
                    liquid_matches = re.findall(liquid_content_pattern, cleaned_line)
                    for match in liquid_matches:
                        if '.join(' in match and not any(indicator in line for indicator in ['f"', "f'", 'return', 'print']):
                            file_errors.append({
                                'file': str(filepath),
                                'line': line_num,
                                'error': 'Python-style formatting within Liquid tag',
                                'content': line.strip()
                            })
                
                # Check for unclosed tag blocks {% without %}
                tag_opens = [(m.start(), m.end()) for m in re.finditer(r'\{%', cleaned_line)]
                tag_closes = [(m.start(), m.end()) for m in re.finditer(r'%\}', cleaned_line)]
                
                if len(tag_opens) != len(tag_closes):
                    file_errors.append({
                        'file': str(filepath),
                        'line': line_num,
                        'error': 'Mismatched {% %} tags',
                        'content': line.strip()
                    })
            
            # Check for multi-line Liquid blocks (only outside code blocks)
            block_stack = []
            block_patterns = {
                'if': r'\{%\s*if\s+',
                'for': r'\{%\s*for\s+',
                'unless': r'\{%\s*unless\s+',
                'case': r'\{%\s*case\s+',
                'capture': r'\{%\s*capture\s+',
                'comment': r'\{%\s*comment\s*%\}',
                'raw': r'\{%\s*raw\s*%\}'
            }
            
            end_patterns = {
                'endif': r'\{%\s*endif\s*%\}',
                'endfor': r'\{%\s*endfor\s*%\}',
                'endunless': r'\{%\s*endunless\s*%\}',
                'endcase': r'\{%\s*endcase\s*%\}',
                'endcapture': r'\{%\s*endcapture\s*%\}',
                'endcomment': r'\{%\s*endcomment\s*%\}',
                'endraw': r'\{%\s*endraw\s*%\}'
            }
            
            in_code_block = False
            for line_num, line in enumerate(lines, 1):
                # Track code blocks
                if line.strip().startswith('```'):
                    in_code_block = not in_code_block
                    continue
                    
                if in_code_block:
                    continue
                
                # Check for opening blocks
                for block_type, pattern in block_patterns.items():
                    if re.search(pattern, line):
                        block_stack.append((block_type, line_num))
                
                # Check for closing blocks
                for end_type, pattern in end_patterns.items():
                    if re.search(pattern, line):
                        expected_type = end_type.replace('end', '')
                        if not block_stack:
                            file_errors.append({
                                'file': str(filepath),
                                'line': line_num,
                                'error': f'Unexpected {{% {end_type} %}} without opening block',
                                'content': line.strip()
                            })
                        elif block_stack[-1][0] != expected_type:
                            file_errors.append({
                                'file': str(filepath),
                                'line': line_num,
                                'error': f'Mismatched block: expected end{block_stack[-1][0]}, found {end_type}',
                                'content': line.strip()
                            })
                        else:
                            block_stack.pop()
            
            # Check for unclosed blocks at end of file
            for block_type, line_num in block_stack:
                file_errors.append({
                    'file': str(filepath),
                    'line': line_num,
                    'error': f'Unclosed {{% {block_type} %}} block',
                    'content': f'Block opened at line {line_num}'
                })
                
        except Exception as e:
            file_errors.append({
                'file': str(filepath),
                'line': 0,
                'error': f'Error reading file: {str(e)}',
                'content': ''
            })
            
        return file_errors

def main():
    validator = LiquidSyntaxValidator()
    docs_dir = Path('/home/anavarre/Projects/eiplgrader/docs')
    
    # Find all markdown files
    md_files = list(docs_dir.rglob('*.md'))
    md_files.sort()
    
    print(f"Validating {len(md_files)} markdown files for Jekyll/Liquid syntax errors...\n")
    print("Note: This validator ignores content inside code blocks and inline code.\n")
    
    all_errors = []
    files_with_errors = 0
    
    for md_file in md_files:
        errors = validator.validate_file(md_file)
        if errors:
            files_with_errors += 1
            all_errors.extend(errors)
    
    # Print results
    if all_errors:
        print(f"Found {len(all_errors)} potential syntax errors in {files_with_errors} files:\n")
        
        current_file = None
        for error in all_errors:
            if error['file'] != current_file:
                current_file = error['file']
                print(f"\n{current_file}:")
            
            print(f"  Line {error['line']}: {error['error']}")
            if error['content']:
                print(f"    Content: {error['content']}")
    else:
        print("✓ No Jekyll/Liquid syntax errors found!")
    
    print(f"\nSummary:")
    print(f"  Total files checked: {len(md_files)}")
    print(f"  Files with errors: {files_with_errors}")
    print(f"  Total errors found: {len(all_errors)}")
    
    # Additional check for the specific error mentioned in the Jekyll error
    print("\n\nSearching for the specific malformed syntax pattern...")
    for md_file in md_files:
        try:
            with open(md_file, 'r', encoding='utf-8') as f:
                content = f.read()
                lines = content.split('\n')
                
            for line_num, line in enumerate(lines, 1):
                # Look for the specific pattern from the error
                if '{{ {chr(10).join(param_decls)}' in line:
                    print(f"\nFOUND EXACT MATCH:")
                    print(f"  File: {md_file}")
                    print(f"  Line {line_num}: {line.strip()}")
                    
        except Exception:
            pass
    
    return 1 if all_errors else 0

if __name__ == '__main__':
    sys.exit(main())