---
layout: default
title: Quickstart Guides
nav_order: 2
has_children: true
permalink: /quickstart/
---

# Quickstart Guides

Get up and running with EiplGrader in your preferred programming language.

## Choose Your Language

<div class="language-grid">
  <div class="language-card">
    <h3>🐍 Python</h3>
    <p>Dynamic typing with automatic inference</p>
    <a href="python.html" class="btn btn-primary">Python Quickstart →</a>
  </div>
  
  <div class="language-card">
    <h3>🟨 JavaScript</h3>
    <p>Node.js with async/await support</p>
    <a href="javascript.html" class="btn btn-primary">JavaScript Quickstart →</a>
  </div>
  
  <div class="language-card">
    <h3>☕ Java</h3>
    <p>Static typing with explicit annotations</p>
    <a href="java.html" class="btn btn-primary">Java Quickstart →</a>
  </div>
  
  <div class="language-card">
    <h3>⚡ C/C++</h3>
    <p>System programming languages</p>
    <a href="c-cpp.html" class="btn btn-primary">C/C++ Quickstart →</a>
  </div>
  
  <div class="language-card">
    <h3>🔷 Go</h3>
    <p>Modern compiled language</p>
    <a href="go.html" class="btn btn-primary">Go Quickstart →</a>
  </div>
  
  <div class="language-card">
    <h3>λ Haskell</h3>
    <p>Pure functional programming</p>
    <a href="haskell.html" class="btn btn-primary">Haskell Quickstart →</a>
  </div>
</div>

## Quick Comparison

| Language | Type System | Type Inference | Best For |
|----------|-------------|----------------|----------|
| Python | Dynamic | ✅ Automatic | Beginners, rapid prototyping |
| JavaScript | Dynamic | ✅ Automatic | Web developers, async code |
| Java | Static | ❌ Required | Enterprise, OOP concepts |
| C/C++ | Static | ❌ Required | Systems programming, performance |
| Go | Static | ❌ Required | Concurrent programming, simplicity |
| Haskell | Static | ❌ Required | Functional programming, theory |

## Common Steps

All quickstart guides follow the same pattern:

1. **Install EiplGrader**
   ```bash
   pip install eiplgrader
   ```

2. **Set up API key**
   ```bash
   export OPENAI_API_KEY="your-key-here"
   ```

3. **Generate code** from natural language

4. **Test the code** with predefined test cases

5. **Analyze results** and iterate

Choose a language above to see specific examples and patterns.

<style>
.language-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1.5rem;
  margin: 2rem 0;
}

.language-card {
  border: 1px solid var(--color-border);
  border-radius: 8px;
  padding: 1.5rem;
  text-align: center;
  transition: transform 0.2s;
}

.language-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
}

.language-card h3 {
  margin-top: 0;
  margin-bottom: 0.5rem;
}

.language-card p {
  color: var(--color-text-secondary);
  margin-bottom: 1rem;
}
</style>