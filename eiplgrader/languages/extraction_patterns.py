"""Documentation and registry of code extraction patterns.

This module documents all extraction patterns used across different programming
languages and provides a centralized registry for pattern management.
"""

from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum
import re

from .code_extractor import ExtractionPattern


class PatternType(Enum):
    """Types of extraction patterns."""
    MARKDOWN_DELIMITER = "markdown_delimiter"
    FUNCTION_DEFINITION = "function_definition"
    CLASS_DEFINITION = "class_definition"
    METHOD_DEFINITION = "method_definition"
    STATEMENT_BLOCK = "statement_block"
    MODULE_DEFINITION = "module_definition"


@dataclass
class PatternDocumentation:
    """Documentation for an extraction pattern."""
    pattern: ExtractionPattern
    pattern_type: PatternType
    examples: List[str] = field(default_factory=list)
    edge_cases: List[str] = field(default_factory=list)
    limitations: List[str] = field(default_factory=list)
    success_rate: Optional[float] = None  # From empirical testing


class ExtractionPatternRegistry:
    """Registry for all extraction patterns with documentation."""
    
    def __init__(self):
        """Initialize the pattern registry."""
        self.patterns = self._build_pattern_registry()
        
    def _build_pattern_registry(self) -> Dict[str, Dict[str, PatternDocumentation]]:
        """Build the complete pattern registry with documentation."""
        registry = {}
        
        # Python patterns
        registry["python"] = {
            "function_definitions": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="function_definitions",
                    pattern=r"(def\s+\w+\s*\([^)]*\):.*?)(?=def\s+\w+\s*\(|$)",
                    description="Match Python function definitions"
                ),
                pattern_type=PatternType.FUNCTION_DEFINITION,
                examples=[
                    "def add(a, b):\n    return a + b",
                    "def fibonacci(n):\n    if n <= 1:\n        return n\n    return fibonacci(n-1) + fibonacci(n-2)",
                    "def process_data(data, callback=None):\n    '''Process data with optional callback'''\n    result = []\n    for item in data:\n        processed = item * 2\n        if callback:\n            processed = callback(processed)\n        result.append(processed)\n    return result"
                ],
                edge_cases=[
                    "def func_with_nested_def():\n    def inner():\n        pass\n    return inner",
                    "def decorator_function(func):\n    def wrapper(*args, **kwargs):\n        return func(*args, **kwargs)\n    return wrapper"
                ],
                limitations=[
                    "May not handle complex nested function definitions correctly",
                    "Struggles with functions that have string literals containing 'def'"
                ]
            )
        }
        
        # Java patterns
        registry["java"] = {
            "class_definitions": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="class_definitions",
                    pattern=r"(public\s+class\s+\w+\s*\{[^}]*\})",
                    description="Match Java class definitions"
                ),
                pattern_type=PatternType.CLASS_DEFINITION,
                examples=[
                    "public class Solution {\n    public static int add(int a, int b) {\n        return a + b;\n    }\n}",
                    "public class Calculator {\n    private int value;\n    \n    public Calculator(int initial) {\n        this.value = initial;\n    }\n    \n    public int getValue() {\n        return value;\n    }\n}"
                ],
                edge_cases=[
                    "public class Nested {\n    public class Inner {\n        int x;\n    }\n}",
                    "public class Generic<T> {\n    private T data;\n}"
                ],
                limitations=[
                    "Simple brace counting may fail with complex nested structures",
                    "Does not handle multi-class files well"
                ]
            ),
            "method_definitions": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="method_definitions",
                    pattern=r"(public\s+static\s+\w+\s+\w+\s*\([^)]*\)\s*\{[^}]*\})",
                    description="Match Java method definitions"
                ),
                pattern_type=PatternType.METHOD_DEFINITION,
                examples=[
                    "public static int add(int a, int b) {\n    return a + b;\n}",
                    "public static String reverse(String input) {\n    return new StringBuilder(input).reverse().toString();\n}"
                ],
                edge_cases=[
                    "public static void method_with_nested_blocks() {\n    if (true) {\n        System.out.println(\"nested\");\n    }\n}",
                    "public static <T> List<T> generic_method(T item) {\n    return Arrays.asList(item);\n}"
                ],
                limitations=[
                    "Only matches public static methods",
                    "Brace counting issues with complex nested structures"
                ]
            )
        }
        
        # C patterns
        registry["c"] = {
            "function_definitions": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="function_definitions",
                    pattern=r"(\w+\s+\w+\s*\([^)]*\)[^{]*{[^}]*})",
                    description="Match C function definitions"
                ),
                pattern_type=PatternType.FUNCTION_DEFINITION,
                examples=[
                    "int add(int a, int b) {\n    return a + b;\n}",
                    "void print_array(int arr[], int size) {\n    for(int i = 0; i < size; i++) {\n        printf(\"%d \", arr[i]);\n    }\n}",
                    "char* reverse_string(char* str) {\n    int len = strlen(str);\n    for(int i = 0; i < len/2; i++) {\n        char temp = str[i];\n        str[i] = str[len-1-i];\n        str[len-1-i] = temp;\n    }\n    return str;\n}"
                ],
                edge_cases=[
                    "int function_with_nested_blocks(int x) {\n    if (x > 0) {\n        if (x > 10) {\n            return x * 2;\n        }\n    }\n    return x;\n}",
                    "static inline int optimized_function(int a) {\n    return a * a;\n}"
                ],
                limitations=[
                    "May not handle function pointers correctly",
                    "Struggles with complex pointer syntax",
                    "Simple brace counting can fail with string literals containing braces"
                ]
            )
        }
        
        # C++ patterns
        registry["cpp"] = {
            "function_definitions": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="function_definitions",
                    pattern=r"((?:(?:inline|static|extern|virtual|const|constexpr)\s+)*(?:\w+(?:::\w+)*(?:<[^>]+>)?(?:\s*[*&]+)?)\s+\w+\s*\([^)]*\)[^{]*{[^}]*})",
                    description="Match C++ function definitions"
                ),
                pattern_type=PatternType.FUNCTION_DEFINITION,
                examples=[
                    "int add(int a, int b) {\n    return a + b;\n}",
                    "template<typename T>\nT maximum(T a, T b) {\n    return (a > b) ? a : b;\n}",
                    "std::vector<int> process_vector(const std::vector<int>& input) {\n    std::vector<int> result;\n    for(const auto& item : input) {\n        result.push_back(item * 2);\n    }\n    return result;\n}"
                ],
                edge_cases=[
                    "auto lambda_function = [](int x) -> int {\n    return x * x;\n};",
                    "constexpr int compile_time_function(int n) {\n    return n * n;\n}",
                    "virtual std::unique_ptr<Base> factory_method() override {\n    return std::make_unique<Derived>();\n}"
                ],
                limitations=[
                    "Complex template syntax may not be fully captured",
                    "Lambda functions require special handling",
                    "Member function definitions outside class need special care"
                ]
            )
        }
        
        # JavaScript patterns
        registry["javascript"] = {
            "function_definitions": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="function_definitions",
                    pattern=r"(function\s+\w+\s*\([^)]*\)\s*{[^}]*})",
                    description="Match regular function definitions"
                ),
                pattern_type=PatternType.FUNCTION_DEFINITION,
                examples=[
                    "function add(a, b) {\n    return a + b;\n}",
                    "function fibonacci(n) {\n    if (n <= 1) return n;\n    return fibonacci(n-1) + fibonacci(n-2);\n}"
                ],
                edge_cases=[
                    "function outer() {\n    function inner() {\n        return 'nested';\n    }\n    return inner;\n}",
                    "function* generator() {\n    yield 1;\n    yield 2;\n}"
                ],
                limitations=[
                    "Does not capture arrow functions",
                    "May miss function expressions assigned to variables"
                ]
            ),
            "arrow_functions": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="arrow_functions",
                    pattern=r"((?:const|let|var)\s+\w+\s*=\s*(?:\([^)]*\)|[^=>\s]+)\s*=>\s*(?:{[^}]*}|[^;]+);?)",
                    description="Match arrow function definitions"
                ),
                pattern_type=PatternType.FUNCTION_DEFINITION,
                examples=[
                    "const add = (a, b) => a + b;",
                    "const multiply = (x, y) => {\n    return x * y;\n};",
                    "const square = x => x * x;"
                ],
                edge_cases=[
                    "const complex = (a, b) => {\n    const result = a + b;\n    return result > 0 ? result : 0;\n};",
                    "const async_func = async (data) => {\n    const result = await processData(data);\n    return result;\n};"
                ],
                limitations=[
                    "Complex arrow function syntax may not be fully captured",
                    "May struggle with nested arrow functions"
                ]
            )
        }
        
        # Haskell patterns
        registry["haskell"] = {
            "function_with_signature": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="function_with_signature",
                    pattern=r"(\w+\s*::[^\n]+\n\w+[^=]*=[^}]+?)(?=\n\w+\s*::|$)",
                    description="Match Haskell functions with type signatures"
                ),
                pattern_type=PatternType.FUNCTION_DEFINITION,
                examples=[
                    "add :: Int -> Int -> Int\nadd x y = x + y",
                    "factorial :: Integer -> Integer\nfactorial 0 = 1\nfactorial n = n * factorial (n - 1)",
                    "map :: (a -> b) -> [a] -> [b]\nmap _ [] = []\nmap f (x:xs) = f x : map f xs"
                ],
                edge_cases=[
                    "complex :: (Num a, Ord a) => a -> a -> a\ncomplex x y\n  | x > y = x\n  | otherwise = y",
                    "polymorphic :: forall a. Show a => a -> String\npolymorphic x = show x"
                ],
                limitations=[
                    "May not handle multi-line type signatures correctly",
                    "Complex type constraints can cause parsing issues"
                ]
            ),
            "simple_function": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="simple_function",
                    pattern=r"(\w+\s+[^=]+=.*?)(?=\n\w+\s+[^=]+=|$)",
                    description="Match simple Haskell function definitions"
                ),
                pattern_type=PatternType.FUNCTION_DEFINITION,
                examples=[
                    "double x = x * 2",
                    "isEven n = n `mod` 2 == 0",
                    "head (x:_) = x"
                ],
                edge_cases=[
                    "guards x\n  | x > 0 = \"positive\"\n  | x < 0 = \"negative\"\n  | otherwise = \"zero\"",
                    "pattern [] = \"empty\"\npattern [x] = \"single\"\npattern xs = \"multiple\""
                ],
                limitations=[
                    "May not capture all pattern matching cases",
                    "Guard clauses can be problematic"
                ]
            )
        }
        
        # SQL patterns
        registry["sql"] = {
            "sql_statements": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="sql_statements",
                    pattern=r"((?:SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER).*?)(?=(?:SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER)|$)",
                    flags=re.DOTALL | re.IGNORECASE,
                    description="Match SQL statements"
                ),
                pattern_type=PatternType.STATEMENT_BLOCK,
                examples=[
                    "SELECT * FROM users WHERE age > 18",
                    "INSERT INTO products (name, price) VALUES ('Widget', 10.99)",
                    "UPDATE users SET status = 'active' WHERE last_login > '2023-01-01'",
                    "CREATE TABLE orders (\n    id INT PRIMARY KEY,\n    user_id INT,\n    total DECIMAL(10,2)\n)"
                ],
                edge_cases=[
                    "SELECT u.name, COUNT(o.id) as order_count\nFROM users u\nLEFT JOIN orders o ON u.id = o.user_id\nGROUP BY u.id\nHAVING COUNT(o.id) > 5",
                    "WITH RECURSIVE fibonacci(n, a, b) AS (\n    SELECT 1, 0, 1\n    UNION ALL\n    SELECT n+1, b, a+b FROM fibonacci WHERE n < 10\n)\nSELECT * FROM fibonacci"
                ],
                limitations=[
                    "May not handle complex nested queries well",
                    "String literals containing SQL keywords can cause issues"
                ]
            )
        }
        
        # Rust patterns
        registry["rust"] = {
            "function_definitions": PatternDocumentation(
                pattern=ExtractionPattern(
                    name="function_definitions",
                    pattern=r"(fn\s+\w+\s*(?:<[^>]*>)?\s*\([^)]*\)[^{]*{.*?})",
                    description="Match Rust function definitions"
                ),
                pattern_type=PatternType.FUNCTION_DEFINITION,
                examples=[
                    "fn add(a: i32, b: i32) -> i32 {\n    a + b\n}",
                    "fn fibonacci(n: u32) -> u32 {\n    match n {\n        0 | 1 => n,\n        _ => fibonacci(n - 1) + fibonacci(n - 2),\n    }\n}",
                    "fn process_vec<T: Clone>(vec: Vec<T>) -> Vec<T> {\n    vec.into_iter().map(|x| x.clone()).collect()\n}"
                ],
                edge_cases=[
                    "fn complex_return() -> Result<String, Box<dyn std::error::Error>> {\n    Ok(\"success\".to_string())\n}",
                    "fn generic_function<T: Display + Clone>(item: T) -> String {\n    format!(\"{}\", item)\n}"
                ],
                limitations=[
                    "Complex generic syntax may not be fully captured",
                    "Lifetime parameters need special handling",
                    "Closure definitions are not captured by this pattern"
                ]
            )
        }
        
        return registry
        
    def get_pattern_documentation(self, language: str, pattern_name: str) -> Optional[PatternDocumentation]:
        """Get documentation for a specific pattern."""
        return self.patterns.get(language, {}).get(pattern_name)
        
    def get_language_patterns(self, language: str) -> Dict[str, PatternDocumentation]:
        """Get all patterns for a language."""
        return self.patterns.get(language, {})
        
    def get_patterns_by_type(self, pattern_type: PatternType) -> List[Tuple[str, str, PatternDocumentation]]:
        """Get all patterns of a specific type across all languages."""
        result = []
        for language, patterns in self.patterns.items():
            for pattern_name, doc in patterns.items():
                if doc.pattern_type == pattern_type:
                    result.append((language, pattern_name, doc))
        return result
        
    def test_pattern(self, language: str, pattern_name: str, test_string: str) -> bool:
        """Test if a pattern matches a given string."""
        doc = self.get_pattern_documentation(language, pattern_name)
        if not doc:
            return False
            
        pattern = doc.pattern.pattern
        flags = doc.pattern.flags
        return bool(re.search(pattern, test_string, flags))
        
    def get_pattern_statistics(self) -> Dict[str, int]:
        """Get statistics about the pattern registry."""
        stats = {
            "total_languages": len(self.patterns),
            "total_patterns": sum(len(patterns) for patterns in self.patterns.values()),
            "patterns_by_type": {}
        }
        
        for pattern_type in PatternType:
            count = len(self.get_patterns_by_type(pattern_type))
            stats["patterns_by_type"][pattern_type.value] = count
            
        return stats
        
    def validate_pattern_registry(self) -> List[str]:
        """Validate the pattern registry and return any issues found."""
        issues = []
        
        for language, patterns in self.patterns.items():
            for pattern_name, doc in patterns.items():
                # Test pattern against examples
                for i, example in enumerate(doc.examples):
                    if not self.test_pattern(language, pattern_name, example):
                        issues.append(f"{language}.{pattern_name}: Example {i+1} does not match pattern")
                        
                # Test pattern syntax
                try:
                    re.compile(doc.pattern.pattern, doc.pattern.flags)
                except re.error as e:
                    issues.append(f"{language}.{pattern_name}: Invalid regex - {e}")
                    
        return issues


# Global registry instance
extraction_patterns = ExtractionPatternRegistry()


def get_extraction_patterns() -> ExtractionPatternRegistry:
    """Get the global extraction pattern registry."""
    return extraction_patterns


def document_pattern_usage(language: str, pattern_name: str, success: bool, 
                         input_text: str = "", extracted_code: str = "") -> None:
    """Document pattern usage for analytics (placeholder for future implementation)."""
    # This could be extended to log pattern usage statistics
    # for continuous improvement of extraction patterns
    pass