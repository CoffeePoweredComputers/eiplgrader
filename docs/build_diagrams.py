#!/usr/bin/env python3
"""
Automated Mermaid diagram extraction and generation for Jekyll documentation.

This script:
1. Scans all .md files for ```mermaid code blocks
2. Extracts each diagram to individual .mmd files
3. Generates static SVG/PNG images using mermaid-cli
4. Optionally replaces mermaid code blocks with image references

Usage:
    python build_diagrams.py [--replace] [--format svg|png]
"""

import re
import os
import subprocess
import argparse
from pathlib import Path
from typing import List, Tuple, Dict

class MermaidDiagramProcessor:
    def __init__(self, docs_root: str = ".", output_format: str = "svg"):
        self.docs_root = Path(docs_root)
        self.diagrams_dir = self.docs_root / "assets" / "diagrams"
        self.mermaid_src_dir = self.docs_root / "mermaid_src"
        self.output_format = output_format
        
        # Create directories if they don't exist
        self.diagrams_dir.mkdir(parents=True, exist_ok=True)
        self.mermaid_src_dir.mkdir(parents=True, exist_ok=True)
    
    def find_markdown_files(self) -> List[Path]:
        """Find all markdown files in the docs directory."""
        markdown_files = []
        for pattern in ["**/*.md", "**/*.markdown"]:
            markdown_files.extend(self.docs_root.glob(pattern))
        
        # Exclude site build directory
        return [f for f in markdown_files if "_site" not in str(f)]
    
    def extract_mermaid_blocks(self, file_path: Path) -> List[Tuple[str, str, int]]:
        """Extract mermaid code blocks from a markdown file.
        
        Returns:
            List of tuples: (diagram_content, diagram_id, line_number)
        """
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Pattern to match ```mermaid blocks
        pattern = r'```mermaid\s*\n(.*?)\n```'
        matches = re.finditer(pattern, content, re.DOTALL | re.MULTILINE)
        
        diagrams = []
        for i, match in enumerate(matches):
            diagram_content = match.group(1).strip()
            # Create diagram ID from file path and index
            relative_path = file_path.relative_to(self.docs_root)
            diagram_id = f"{relative_path.stem}_diagram_{i+1}"
            line_number = content[:match.start()].count('\n') + 1
            
            diagrams.append((diagram_content, diagram_id, line_number))
        
        return diagrams
    
    def save_mermaid_file(self, diagram_content: str, diagram_id: str) -> Path:
        """Save diagram content to a .mmd file."""
        mmd_file = self.mermaid_src_dir / f"{diagram_id}.mmd"
        with open(mmd_file, 'w', encoding='utf-8') as f:
            f.write(diagram_content)
        return mmd_file
    
    def generate_diagram_image(self, mmd_file: Path, diagram_id: str) -> bool:
        """Generate SVG/PNG from mermaid file using mermaid-cli."""
        output_file = self.diagrams_dir / f"{diagram_id}.{self.output_format}"
        
        cmd = [
            "mmdc",
            "-i", str(mmd_file),
            "-o", str(output_file),
            "-t", "dark"  # Use dark theme to match site
        ]
        
        # Add format-specific options
        if self.output_format == "png":
            cmd.extend(["-w", "1200", "-H", "800"])
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, check=True)
            print(f"✓ Generated: {output_file}")
            return True
        except subprocess.CalledProcessError as e:
            print(f"✗ Failed to generate {output_file}: {e.stderr}")
            return False
        except FileNotFoundError:
            print("✗ mermaid-cli (mmdc) not found. Install with: npm install -g @mermaid-js/mermaid-cli")
            return False
    
    def replace_mermaid_blocks(self, file_path: Path, diagrams: List[Tuple[str, str, int]]) -> bool:
        """Replace mermaid code blocks with image references."""
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Replace from bottom to top to preserve line numbers
        for diagram_content, diagram_id, _ in reversed(diagrams):
            # Find the exact block to replace
            block_pattern = r'```mermaid\s*\n' + re.escape(diagram_content) + r'\n```'
            
            # Create image reference
            image_ref = f"![{diagram_id.replace('_', ' ').title()}](/assets/diagrams/{diagram_id}.{self.output_format})"
            
            content = re.sub(block_pattern, image_ref, content, count=1)
        
        # Write back to file
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        return True
    
    def process_all_files(self, replace_blocks: bool = False) -> Dict[str, int]:
        """Process all markdown files and generate diagrams."""
        stats = {"files_processed": 0, "diagrams_found": 0, "diagrams_generated": 0}
        
        markdown_files = self.find_markdown_files()
        print(f"Found {len(markdown_files)} markdown files")
        
        for md_file in markdown_files:
            print(f"\nProcessing: {md_file.relative_to(self.docs_root)}")
            
            # Extract mermaid blocks
            diagrams = self.extract_mermaid_blocks(md_file)
            if not diagrams:
                print("  No mermaid diagrams found")
                continue
            
            stats["files_processed"] += 1
            stats["diagrams_found"] += len(diagrams)
            
            # Process each diagram
            file_diagrams = []
            for diagram_content, diagram_id, line_number in diagrams:
                print(f"  Found diagram at line {line_number}: {diagram_id}")
                
                # Save .mmd file
                mmd_file = self.save_mermaid_file(diagram_content, diagram_id)
                
                # Generate image
                if self.generate_diagram_image(mmd_file, diagram_id):
                    stats["diagrams_generated"] += 1
                    file_diagrams.append((diagram_content, diagram_id, line_number))
            
            # Replace blocks if requested
            if replace_blocks and file_diagrams:
                self.replace_mermaid_blocks(md_file, file_diagrams)
                print(f"  Replaced {len(file_diagrams)} mermaid blocks with image references")
        
        return stats


def main():
    parser = argparse.ArgumentParser(description="Process Mermaid diagrams in Jekyll documentation")
    parser.add_argument("--replace", action="store_true", 
                       help="Replace mermaid code blocks with image references")
    parser.add_argument("--format", choices=["svg", "png"], default="svg",
                       help="Output image format (default: svg)")
    parser.add_argument("--docs-root", default=".",
                       help="Root directory of documentation (default: current directory)")
    
    args = parser.parse_args()
    
    processor = MermaidDiagramProcessor(args.docs_root, args.format)
    
    print(f"Mermaid Diagram Processor")
    print(f"========================")
    print(f"Docs root: {args.docs_root}")
    print(f"Output format: {args.format}")
    print(f"Replace blocks: {args.replace}")
    print()
    
    # Check if mermaid-cli is available
    try:
        subprocess.run(["mmdc", "--version"], capture_output=True, check=True)
    except (subprocess.CalledProcessError, FileNotFoundError):
        print("ERROR: mermaid-cli not found!")
        print("Install with: npm install -g @mermaid-js/mermaid-cli")
        return 1
    
    # Process files
    stats = processor.process_all_files(args.replace)
    
    print(f"\nSummary:")
    print(f"========")
    print(f"Files processed: {stats['files_processed']}")
    print(f"Diagrams found: {stats['diagrams_found']}")
    print(f"Diagrams generated: {stats['diagrams_generated']}")
    
    if stats['diagrams_generated'] > 0:
        print(f"\nGenerated diagrams are in: assets/diagrams/")
        if not args.replace:
            print(f"To use in markdown:")
            print(f"![Diagram Description](/assets/diagrams/filename.{args.format})")
    
    return 0


if __name__ == "__main__":
    exit(main())