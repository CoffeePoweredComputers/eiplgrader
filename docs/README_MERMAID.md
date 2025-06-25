# Mermaid Diagram Solutions for Jekyll

This document outlines the **4 alternative approaches** for rendering Mermaid diagrams when client-side JavaScript fails.

## 🚨 Problem
Client-side Mermaid rendering wasn't working due to:
- Version conflicts between Mermaid versions
- Jekyll theme integration issues  
- JavaScript initialization timing problems

## ✅ Solution 1: Automated Static Generation (Recommended)

**Use the automated Python script to convert all Mermaid diagrams to static images:**

```bash
# Install mermaid-cli (one-time setup)
npm install -g @mermaid-js/mermaid-cli

# Generate all diagrams as SVG
python3 build_diagrams.py --format svg

# Generate as PNG if you prefer
python3 build_diagrams.py --format png

# Replace mermaid code blocks with image references
python3 build_diagrams.py --replace --format svg
```

**Features:**
- ✅ Scans all `.md` files for ```mermaid blocks automatically
- ✅ Extracts each diagram to individual `.mmd` files  
- ✅ Generates static SVG/PNG with dark theme
- ✅ Optionally replaces code blocks with image references
- ✅ Works in any environment (no JavaScript dependencies)
- ✅ Perfect for GitHub Pages and static hosting

**File Structure:**
```
docs/
├── build_diagrams.py          # Automation script
├── mermaid_src/               # Generated .mmd files
│   ├── architecture_diagram_1.mmd
│   └── ...
└── assets/diagrams/           # Generated images
    ├── architecture_diagram_1.svg
    └── ...
```

## ✅ Solution 2: Manual Static Generation

**For individual diagrams:**

```bash
# Create .mmd file with your diagram
echo "graph TD; A-->B" > my-diagram.mmd

# Generate SVG
mmdc -i my-diagram.mmd -o assets/diagrams/my-diagram.svg -t dark

# Use in markdown
![My Diagram](/assets/diagrams/my-diagram.svg)
```

## ✅ Solution 3: Alternative Static Site Generators

**Modern SSGs with better Mermaid support:**

- **Astro**: Native Mermaid support, fast builds
- **Next.js**: Server-side rendering, React ecosystem  
- **Hugo**: Excellent performance, Go-based
- **MkDocs**: Documentation-focused, Python-based

## ✅ Solution 4: Jekyll Plugin (Limited)

**For sites that support custom plugins:**

```yaml
# _config.yml
plugins:
  - jekyll-mermaid

# Usage in markdown
{% mermaid %}
graph TD
    A --> B
{% endmermaid %}
```

**Note:** GitHub Pages doesn't support custom plugins.

## 🎯 Recommendation

**Use Solution 1 (Automated Static Generation)** because:
- ✅ Works everywhere (GitHub Pages, Netlify, Vercel)
- ✅ No JavaScript dependencies or loading issues
- ✅ Fast page loads (images vs client-side rendering)
- ✅ SEO-friendly (images are crawlable)
- ✅ Fully automated workflow
- ✅ Version control friendly (.mmd source files)

## 📋 Migration Workflow

1. **Run the automation script:**
   ```bash
   python3 build_diagrams.py --format svg
   ```

2. **Review generated diagrams** in `assets/diagrams/`

3. **Optional: Replace code blocks automatically:**
   ```bash
   python3 build_diagrams.py --replace --format svg
   ```

4. **Add to build process** (e.g., GitHub Actions):
   ```yaml
   - name: Generate Mermaid Diagrams
     run: |
       npm install -g @mermaid-js/mermaid-cli
       python3 build_diagrams.py --format svg
   ```

The static generation approach provides the most reliable, performant, and maintainable solution for Mermaid diagrams in Jekyll.