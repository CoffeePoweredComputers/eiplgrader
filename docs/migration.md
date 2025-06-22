---
layout: default
title: Migration Guide
nav_order: 6
permalink: /migration/
---

# Migration Guide

Guide for migrating between EiplGrader versions and from other grading systems.

## Version Migration

### Migrating to v2.0

Version 2.0 introduces significant changes to support multiple languages and improved architecture.

#### Breaking Changes

1. **Import Path Changes**
   ```python
   # Old (v1.x)
   from eiplgrader import generate_code, test_code
   
   # New (v2.0)
   from eiplgrader import CodeGenerator, CodeTester
   ```

2. **API Changes**
   ```python
   # Old (v1.x)
   result = generate_code(
       prompt="calculate factorial",
       function_name="factorial"
   )
   
   # New (v2.0)
   generator = CodeGenerator(api_key)
   result = generator.generate_code(
       student_response="calculate factorial",
       function_name="factorial"
   )
   ```

3. **Test Case Format**
   ```python
   # Old (v1.x) - Python only
   test_cases = [
       {"input": {"n": 5}, "output": 120}
   ]
   
   # New (v2.0) - Multi-language
   test_cases = [
       {"parameters": {"n": 5}, "expected": 120}
   ]
   ```

#### Migration Steps

1. **Update Imports**
   ```python
   # Update all imports
   sed -i 's/from eiplgrader import generate_code/from eiplgrader import CodeGenerator/g' *.py
   ```

2. **Update Code Generation**
   ```python
   # Old code
   def grade_submission(prompt, func_name):
       code = generate_code(prompt, func_name)
       return test_code(code, test_cases)
   
   # New code
   def grade_submission(prompt, func_name):
       generator = CodeGenerator(api_key)
       result = generator.generate_code(
           student_response=prompt,
           function_name=func_name
       )
       
       tester = CodeTester(
           code=result.codes[0],
           test_cases=test_cases,
           function_name=func_name
       )
       return tester.run_tests()
   ```

3. **Update Test Cases**
   ```python
   # Migration script for test cases
   def migrate_test_cases(old_tests):
       new_tests = []
       for test in old_tests:
           new_test = {
               "parameters": test.get("input", {}),
               "expected": test.get("output")
           }
           # Add types for static languages
           if language in ["java", "cpp", "go"]:
               new_test["parameter_types"] = infer_types(test["input"])
               new_test["expected_type"] = infer_type(test["output"])
           new_tests.append(new_test)
       return new_tests
   ```

#### New Features in v2.0

- Multi-language support (7 languages)
- Improved error handling
- Parallel test execution
- Docker integration
- Type system support

### Migrating from v0.x to v1.0

If you're still on v0.x, first migrate to v1.0:

```bash
# Install v1.0
pip install eiplgrader==1.0.0

# Run migration script
python -m eiplgrader.migrate_v0_to_v1
```

## Migrating from Other Systems

### From Manual Grading

If you're currently grading manually:

1. **Prepare Test Cases**
   ```python
   # Convert rubric to test cases
   test_cases = []
   
   # Example: Factorial function
   test_cases.extend([
       {"parameters": {"n": 0}, "expected": 1},  # Base case
       {"parameters": {"n": 1}, "expected": 1},  # Base case
       {"parameters": {"n": 5}, "expected": 120}, # Normal case
       {"parameters": {"n": 10}, "expected": 3628800}, # Larger input
   ])
   ```

2. **Create Grading Script**
   ```python
   from eiplgrader import CodeGenerator, CodeTester
   import csv
   
   def grade_submissions(csv_file):
       generator = CodeGenerator(api_key)
       results = []
       
       with open(csv_file) as f:
           reader = csv.DictReader(f)
           for row in reader:
               # Generate code from student response
               gen_result = generator.generate_code(
                   student_response=row['response'],
                   function_name=row['function_name']
               )
               
               # Test the code
               tester = CodeTester(
                   code=gen_result.codes[0],
                   test_cases=load_test_cases(row['question_id']),
                   function_name=row['function_name']
               )
               
               test_results = tester.run_tests()
               
               results.append({
                   'student_id': row['student_id'],
                   'score': test_results.pass_rate,
                   'feedback': generate_feedback(test_results)
               })
       
       return results
   ```

### From CodeRunner (Moodle)

Migrating from CodeRunner to EiplGrader:

1. **Export CodeRunner Questions**
   ```python
   # Parse CodeRunner XML
   import xml.etree.ElementTree as ET
   
   def parse_coderunner_question(xml_file):
       tree = ET.parse(xml_file)
       root = tree.getroot()
       
       test_cases = []
       for testcase in root.findall('.//testcase'):
           test_cases.append({
               "parameters": parse_stdin(testcase.find('stdin').text),
               "expected": testcase.find('expected').text.strip()
           })
       
       return {
           'description': root.find('.//questiontext').text,
           'test_cases': test_cases
       }
   ```

2. **Convert to EiplGrader Format**
   ```python
   def convert_coderunner_to_eiplgrader(cr_question):
       return {
           'student_response': cr_question['description'],
           'test_cases': [
               convert_test_case(tc) for tc in cr_question['test_cases']
           ]
       }
   ```

### From HackerRank/LeetCode Style

Converting from competitive programming platforms:

1. **Test Case Conversion**
   ```python
   def convert_hackerrank_test(input_str, output_str):
       """Convert HackerRank I/O format to EiplGrader."""
       # Parse input (example for array input)
       lines = input_str.strip().split('\n')
       n = int(lines[0])
       arr = list(map(int, lines[1].split()))
       
       return {
           "parameters": {"n": n, "arr": arr},
           "expected": int(output_str.strip())
       }
   ```

2. **Problem Statement Conversion**
   ```python
   def convert_problem_statement(hackerrank_problem):
       # Extract the core task description
       description = extract_task_description(hackerrank_problem['statement'])
       
       # Remove competitive programming specific parts
       description = remove_io_format_details(description)
       description = remove_constraints_section(description)
       
       return description
   ```

## Configuration Migration

### Environment Variables

```bash
# Old configuration
export EIPLGRADER_API_KEY="sk-..."
export EIPLGRADER_MODEL="gpt-3.5-turbo"

# New configuration (v2.0+)
export OPENAI_API_KEY="sk-..."
export EIPLGRADER_DEFAULT_MODEL="openai"
export EIPLGRADER_DEFAULT_LANGUAGE="python"
```

### Configuration Files

Old format (`.eiplgrader.json`):
```json
{
    "api_key": "sk-...",
    "model": "gpt-3.5-turbo",
    "timeout": 10
}
```

New format (`.eiplgrader.yml`):
```yaml
models:
  openai:
    api_key: ${OPENAI_API_KEY}
    default_model: gpt-4
    
execution:
  timeout: 10
  parallel: true
  
languages:
  default: python
  supported:
    - python
    - javascript
    - java
```

## Database Migration

If you're storing results in a database:

### Schema Changes

```sql
-- Old schema
CREATE TABLE submissions (
    id INTEGER PRIMARY KEY,
    student_id TEXT,
    code TEXT,
    score REAL
);

-- New schema
CREATE TABLE submissions (
    id INTEGER PRIMARY KEY,
    student_id TEXT,
    question_id TEXT,
    language TEXT DEFAULT 'python',
    student_response TEXT,  -- Natural language
    generated_code TEXT,    -- Generated code
    test_results JSON,      -- Detailed results
    score REAL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Migration
ALTER TABLE submissions ADD COLUMN language TEXT DEFAULT 'python';
ALTER TABLE submissions ADD COLUMN student_response TEXT;
ALTER TABLE submissions ADD COLUMN test_results JSON;
ALTER TABLE submissions RENAME COLUMN code TO generated_code;
```

### Data Migration Script

```python
import sqlite3
import json

def migrate_database(db_path):
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    
    # Add new columns
    cursor.execute("ALTER TABLE submissions ADD COLUMN language TEXT DEFAULT 'python'")
    cursor.execute("ALTER TABLE submissions ADD COLUMN test_results TEXT")
    
    # Migrate existing data
    cursor.execute("SELECT id, score FROM submissions")
    for row in cursor.fetchall():
        test_results = {
            "all_passed": row[1] == 1.0,
            "pass_rate": row[1] * 100,
            "language": "python"
        }
        
        cursor.execute(
            "UPDATE submissions SET test_results = ? WHERE id = ?",
            (json.dumps(test_results), row[0])
        )
    
    conn.commit()
    conn.close()
```

## Rollback Procedures

If you need to rollback:

### Version Rollback

```bash
# Rollback to previous version
pip install eiplgrader==1.5.0

# Or using git
git checkout v1.5.0
pip install -e .
```

### Data Rollback

Always backup before migration:

```bash
# Backup database
cp grading.db grading.db.backup

# Backup configuration
cp -r ~/.eiplgrader ~/.eiplgrader.backup

# Restore if needed
cp grading.db.backup grading.db
cp -r ~/.eiplgrader.backup ~/.eiplgrader
```

## Troubleshooting Migration

### Common Issues

1. **Import Errors**
   ```python
   # Error: ImportError: cannot import name 'generate_code'
   # Solution: Update imports to use new API
   from eiplgrader import CodeGenerator
   ```

2. **Test Case Format Errors**
   ```python
   # Error: KeyError: 'parameters'
   # Solution: Update test case format
   old_test = {"input": {"x": 5}, "output": 10}
   new_test = {"parameters": {"x": 5}, "expected": 10}
   ```

3. **API Key Issues**
   ```python
   # Error: No API key provided
   # Solution: Update initialization
   generator = CodeGenerator(api_key=os.getenv("OPENAI_API_KEY"))
   ```

### Migration Checklist

- [ ] Backup all data and configuration
- [ ] Update EiplGrader to target version
- [ ] Update all imports in code
- [ ] Convert test case formats
- [ ] Update configuration files
- [ ] Migrate database schema if applicable
- [ ] Run tests to verify functionality
- [ ] Update documentation

## Getting Help

If you encounter issues during migration:

1. Check the [GitHub Issues](https://github.com/hamiltonfour/eiplgrader/issues)
2. Review the [CHANGELOG](https://github.com/hamiltonfour/eiplgrader/blob/main/CHANGELOG.md)
3. Ask in [GitHub Discussions](https://github.com/hamiltonfour/eiplgrader/discussions)
4. Contact maintainers for critical issues

## Next Steps

After migration:
- Review new features in the [User Guide](/guide/)
- Update your workflows to use new capabilities
- Consider contributing improvements back to the project