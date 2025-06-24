#!/usr/bin/env python3
"""Validate Jekyll/Liquid syntax in markdown files."""

import re
import sys
from pathlib import Path
from typing import List, Tuple, Dict

class LiquidSyntaxValidator:
    def __init__(self):
        self.errors: List[Dict[str, any]] = []
        
    def validate_file(self, filepath: Path) -> List[Dict[str, any]]:
        """Validate a single markdown file for Liquid syntax errors."""
        file_errors = []
        
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                content = f.read()
                lines = content.split('\n')
                
            # Check for unclosed variable tags {{ without }}
            for line_num, line in enumerate(lines, 1):
                # Find all {{ occurrences
                var_opens = [(m.start(), m.end()) for m in re.finditer(r'\{\{', line)]
                var_closes = [(m.start(), m.end()) for m in re.finditer(r'\}\}', line)]
                
                # Check if opens match closes
                if len(var_opens) != len(var_closes):
                    file_errors.append({
                        'file': str(filepath),
                        'line': line_num,
                        'error': 'Mismatched {{ }} tags',
                        'content': line.strip()
                    })
                
                # Check for malformed variable syntax
                # Look for patterns like {{ {something
                malformed_pattern = r'\{\{\s*\{[^{]'
                if re.search(malformed_pattern, line):
                    file_errors.append({
                        'file': str(filepath),
                        'line': line_num,
                        'error': 'Malformed variable syntax (extra {)',
                        'content': line.strip()
                    })
                
                # Check for Python-style formatting within Liquid tags
                python_format_pattern = r'\{\{.*\.join\(.*\)\s*\}'
                if re.search(python_format_pattern, line):
                    file_errors.append({
                        'file': str(filepath),
                        'line': line_num,
                        'error': 'Python-style formatting within Liquid tag',
                        'content': line.strip()
                    })
                
                # Check for unclosed tag blocks {% without %}
                tag_opens = [(m.start(), m.end()) for m in re.finditer(r'\{%', line)]
                tag_closes = [(m.start(), m.end()) for m in re.finditer(r'%\}', line)]
                
                if len(tag_opens) != len(tag_closes):
                    file_errors.append({
                        'file': str(filepath),
                        'line': line_num,
                        'error': 'Mismatched {% %} tags',
                        'content': line.strip()
                    })
            
            # Check for multi-line Liquid blocks
            # Look for {% if/for/etc without matching {% endif/endfor/etc %}
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
            
            for line_num, line in enumerate(lines, 1):
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
    
    all_errors = []
    files_with_errors = 0
    
    for md_file in md_files:
        errors = validator.validate_file(md_file)
        if errors:
            files_with_errors += 1
            all_errors.extend(errors)
    
    # Print results
    if all_errors:
        print(f"Found {len(all_errors)} syntax errors in {files_with_errors} files:\n")
        
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
    
    return 1 if all_errors else 0

if __name__ == '__main__':
    sys.exit(main())