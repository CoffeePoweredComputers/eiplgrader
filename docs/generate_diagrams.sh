#!/bin/bash

# Generate static diagrams from Mermaid files for Jekyll site
# Usage: ./generate_diagrams.sh

echo "Generating static diagrams from Mermaid files..."

# Create diagrams directory if it doesn't exist
mkdir -p assets/diagrams

# Process all .mmd files in the docs directory recursively
find . -name "*.mmd" -type f | while read -r mermaid_file; do
    # Get the relative path without extension
    base_name=$(basename "$mermaid_file" .mmd)
    
    # Generate SVG with dark theme
    echo "Processing: $mermaid_file -> assets/diagrams/${base_name}.svg"
    mmdc -i "$mermaid_file" -o "assets/diagrams/${base_name}.svg" -t dark
    
    # Also generate PNG for better compatibility
    echo "Processing: $mermaid_file -> assets/diagrams/${base_name}.png"
    mmdc -i "$mermaid_file" -o "assets/diagrams/${base_name}.png" -t dark -w 1200 -H 800
done

echo "Diagram generation complete!"
echo "Generated diagrams are in: assets/diagrams/"
echo ""
echo "To use in markdown:"
echo "![Diagram Description](/assets/diagrams/filename.svg)"
echo "or"
echo "![Diagram Description](/assets/diagrams/filename.png)"