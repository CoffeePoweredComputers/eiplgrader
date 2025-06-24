---
layout: default
title: Mermaid Test Page
nav_exclude: true
---

# Mermaid Diagram Test

This page tests if mermaid diagrams are working properly.

## Simple Flow Chart

```mermaid
graph TD
    A[Start] --> B{Is it working?}
    B -->|Yes| C[Great!]
    B -->|No| D[Check console]
    C --> E[End]
    D --> E
```

## Architecture Diagram

```mermaid
graph TB
    subgraph "User Layer"
        UI[User Interface]
    end
    
    subgraph "Core Layer"
        CG[CodeGenerator]
        CT[CodeTester]
    end
    
    subgraph "Language Layer"
        LR[LanguageRegistry]
        LA[Language Adapters]
        LE[Language Executors]
    end
    
    UI --> CG
    UI --> CT
    CG --> LR
    CT --> LR
    LR --> LA
    LR --> LE
```

If you can see interactive diagrams above instead of code blocks, mermaid is working correctly!