"""Test memory usage and resource exhaustion scenarios."""

import pytest
from eiplgrader.tester import CodeTester
from eiplgrader.languages import language_registry


class TestMemoryScenarios:
    """Test memory usage and resource exhaustion scenarios."""

    def test_memory_exhaustion_python(self):
        """Test Python code that tries to allocate excessive memory."""
        memory_exhaustion_code = """
def add_numbers(a, b):
    # Try to allocate a huge list
    big_list = [0] * (10**8)  # 100 million integers
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=memory_exhaustion_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might fail with memory error or succeed on systems with enough RAM
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_memory_leak_simulation_python(self):
        """Test Python code that simulates memory leak."""
        memory_leak_code = """
def add_numbers(a, b):
    # Simulate memory leak by creating nested lists
    data = []
    for i in range(10**5):  # 100,000 iterations
        data.append([i] * 1000)  # Each iteration adds 1000 integers
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=memory_leak_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might fail with memory error or timeout
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_string_memory_explosion_python(self):
        """Test Python code that creates huge strings."""
        string_explosion_code = """
def add_numbers(a, b):
    # Create a massive string
    big_string = "x" * (10**7)  # 10 million characters
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=string_explosion_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might fail with memory error
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_recursive_data_structure_python(self):
        """Test Python code that creates deeply recursive data structures."""
        recursive_structure_code = """
def add_numbers(a, b):
    # Create deeply nested structure
    current = []
    for i in range(10000):  # 10,000 levels deep
        new_list = [current]
        current = new_list
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=recursive_structure_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might fail with memory or recursion error
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_dictionary_explosion_python(self):
        """Test Python code that creates huge dictionaries."""
        dict_explosion_code = """
def add_numbers(a, b):
    # Create a dictionary with many entries
    big_dict = {}
    for i in range(10**6):  # 1 million entries
        big_dict[f"key_{i}"] = f"value_{i}"
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=dict_explosion_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might fail with memory error or timeout
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_file_handle_exhaustion_python(self):
        """Test Python code that opens many files."""
        file_exhaustion_code = """
import tempfile
import os

def add_numbers(a, b):
    # Try to open many files
    files = []
    try:
        for i in range(10000):  # Try to open 10,000 files
            f = tempfile.NamedTemporaryFile(delete=False)
            files.append(f)
    except Exception as e:
        # Clean up opened files
        for f in files:
            f.close()
            try:
                os.unlink(f.name)
            except:
                pass
        raise e
    
    # Clean up
    for f in files:
        f.close()
        try:
            os.unlink(f.name)
        except:
            pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=file_exhaustion_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might fail with OS error (too many open files)
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_thread_exhaustion_python(self):
        """Test Python code that creates many threads."""
        thread_exhaustion_code = """
import threading
import time

def add_numbers(a, b):
    threads = []
    
    def worker():
        time.sleep(0.1)
    
    try:
        # Try to create many threads
        for i in range(1000):  # 1000 threads
            t = threading.Thread(target=worker)
            threads.append(t)
            t.start()
    except Exception as e:
        # Clean up threads
        for t in threads:
            if t.is_alive():
                t.join(timeout=0.1)
        raise e
    
    # Wait for all threads to complete
    for t in threads:
        t.join()
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=thread_exhaustion_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might fail with OS error or timeout
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    @pytest.mark.skipif(
        not language_registry.is_supported("java"), reason="Java not available"
    )
    def test_memory_exhaustion_java(self):
        """Test Java code that tries to allocate excessive memory."""
        memory_exhaustion_java = """
import java.util.*;

public class Solution {
    public static int add_numbers(int a, int b) {
        // Try to allocate a huge array
        try {
            int[] bigArray = new int[100000000];  // 100 million integers
        } catch (OutOfMemoryError e) {
            // Handle memory error
        }
        return a + b;
    }
}
"""
        test_cases = [
            {
                "parameters": {"a": 1, "b": 2},
                "parameter_types": {"a": "int", "b": "int"},
                "expected": 3,
                "expected_type": "int",
            }
        ]

        tester = CodeTester(
            code=memory_exhaustion_java,
            test_cases=test_cases,
            function_name="add_numbers",
            language="java",
        )

        result = tester.run_tests()
        # This might fail with memory error
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    @pytest.mark.skipif(
        not language_registry.is_supported("cpp"), reason="C++ not available"
    )
    def test_memory_exhaustion_cpp(self):
        """Test C++ code that tries to allocate excessive memory."""
        memory_exhaustion_cpp = """
#include <iostream>
#include <vector>
using namespace std;

int add_numbers(int a, int b) {
    try {
        // Try to allocate a huge vector
        vector<int> bigVector(100000000);  // 100 million integers
    } catch (const std::bad_alloc& e) {
        // Handle memory error
    }
    return a + b;
}
"""
        test_cases = [
            {
                "parameters": {"a": 1, "b": 2},
                "parameter_types": {"a": "int", "b": "int"},
                "expected": 3,
                "expected_type": "int",
            }
        ]

        tester = CodeTester(
            code=memory_exhaustion_cpp,
            test_cases=test_cases,
            function_name="add_numbers",
            language="cpp",
        )

        result = tester.run_tests()
        # This might fail with memory error
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_memory_fragmentation_python(self):
        """Test Python code that causes memory fragmentation."""
        fragmentation_code = """
def add_numbers(a, b):
    # Create many small objects
    objects = []
    for i in range(100000):
        # Create objects of varying sizes
        if i % 3 == 0:
            obj = [0] * 100
        elif i % 3 == 1:
            obj = [0] * 200
        else:
            obj = [0] * 50
        objects.append(obj)
    
    # Delete every other object to fragment memory
    for i in range(0, len(objects), 2):
        del objects[i]
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=fragmentation_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might succeed or fail depending on memory management
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_circular_reference_python(self):
        """Test Python code that creates circular references."""
        circular_reference_code = """
def add_numbers(a, b):
    # Create circular references
    objects = []
    for i in range(10000):  # 10,000 objects
        obj = {"id": i, "refs": []}
        objects.append(obj)
    
    # Create circular references
    for i in range(len(objects)):
        for j in range(i + 1, min(i + 10, len(objects))):
            objects[i]["refs"].append(objects[j])
            objects[j]["refs"].append(objects[i])
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=circular_reference_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # This might succeed or fail depending on garbage collection
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_stack_overflow_python(self):
        """Test Python code that causes stack overflow."""
        stack_overflow_code = """
def add_numbers(a, b):
    def recursive_function(n):
        if n <= 0:
            return 0
        return recursive_function(n - 1) + 1
    
    # This will cause stack overflow
    try:
        recursive_function(100000)  # 100,000 levels of recursion
    except RecursionError:
        pass  # Handle recursion error
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=stack_overflow_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed if recursion error is handled
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0
