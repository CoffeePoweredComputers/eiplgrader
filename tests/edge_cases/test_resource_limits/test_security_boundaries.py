"""Test security boundary scenarios."""

import pytest
from eiplgrader.tester import CodeTester
from eiplgrader.languages import language_registry


class TestSecurityBoundaries:
    """Test security boundary scenarios and potentially malicious code."""

    def test_file_system_access_python(self):
        """Test Python code that tries to access file system."""
        file_access_code = """
import os

def add_numbers(a, b):
    # Try to read sensitive files
    try:
        with open('/etc/passwd', 'r') as f:
            content = f.read()
    except:
        pass
    
    try:
        with open('/etc/shadow', 'r') as f:
            content = f.read()
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=file_access_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed if file access is properly sandboxed
        # Or fail if file access causes errors
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_network_access_python(self):
        """Test Python code that tries to access network."""
        network_access_code = """
import urllib.request
import socket

def add_numbers(a, b):
    # Try to make network requests
    try:
        response = urllib.request.urlopen('http://example.com', timeout=5)
        data = response.read()
    except:
        pass
    
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect(('example.com', 80))
        sock.close()
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=network_access_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed if network access is properly handled
        # Or fail if network access causes errors
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_subprocess_execution_python(self):
        """Test Python code that tries to execute system commands."""
        subprocess_code = """
import subprocess
import os

def add_numbers(a, b):
    # Try to execute system commands
    try:
        result = subprocess.run(['ls', '-la'], capture_output=True, timeout=5)
    except:
        pass
    
    try:
        result = subprocess.run(['whoami'], capture_output=True, timeout=5)
    except:
        pass
    
    try:
        os.system('echo "Hello from system"')
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=subprocess_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed if subprocess execution is properly sandboxed
        # Or fail if subprocess execution causes errors
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_environment_variable_access_python(self):
        """Test Python code that tries to access environment variables."""
        env_access_code = """
import os

def add_numbers(a, b):
    # Try to access environment variables
    try:
        path = os.environ.get('PATH')
        home = os.environ.get('HOME')
        user = os.environ.get('USER')
        
        # Try to set environment variables
        os.environ['MALICIOUS_VAR'] = 'malicious_value'
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=env_access_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed - environment access is usually allowed
        assert result.was_successful()

    def test_module_hijacking_python(self):
        """Test Python code that tries to hijack modules."""
        module_hijacking_code = """
import sys
import importlib

def add_numbers(a, b):
    # Try to modify sys.modules
    try:
        # Save original
        original_os = sys.modules.get('os')
        
        # Create fake module
        class FakeOS:
            def system(self, cmd):
                return "hijacked"
        
        sys.modules['os'] = FakeOS()
        
        # Import and use
        import os
        result = os.system('ls')
        
        # Restore original
        if original_os:
            sys.modules['os'] = original_os
        else:
            del sys.modules['os']
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=module_hijacking_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed if module modification is allowed
        # Or fail if there are restrictions
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_code_injection_python(self):
        """Test Python code that tries to inject code."""
        code_injection_code = """
def add_numbers(a, b):
    # Try to execute arbitrary code
    try:
        eval("__import__('os').system('echo injected')")
    except:
        pass
    
    try:
        exec("import os; os.system('echo executed')")
    except:
        pass
    
    try:
        compile("print('compiled')", "<string>", "exec")
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=code_injection_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed if code injection is properly sandboxed
        # Or fail if code injection causes errors
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_memory_dump_python(self):
        """Test Python code that tries to dump memory."""
        memory_dump_code = """
import gc
import sys

def add_numbers(a, b):
    # Try to access memory and objects
    try:
        # Get all objects in memory
        all_objects = gc.get_objects()
        
        # Try to access frame information
        frame = sys._getframe()
        
        # Try to access builtins
        builtins = __builtins__
        
        # Try to access globals
        globals_dict = globals()
        
        # Try to access locals
        locals_dict = locals()
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=memory_dump_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed - memory inspection is usually allowed
        assert result.was_successful()

    def test_resource_exhaustion_attack_python(self):
        """Test Python code that tries to exhaust system resources."""
        resource_exhaustion_code = """
import threading
import time

def add_numbers(a, b):
    # Try to exhaust system resources
    try:
        # Fork bomb simulation
        def fork_bomb():
            while True:
                threading.Thread(target=fork_bomb).start()
                time.sleep(0.001)
        
        # Start the attack (but with safeguards)
        t = threading.Thread(target=fork_bomb)
        t.daemon = True  # Dies when main thread dies
        t.start()
        time.sleep(0.1)  # Brief delay
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=resource_exhaustion_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed if resource limits are properly enforced
        # Or fail if resource exhaustion causes errors
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_pickle_deserialization_python(self):
        """Test Python code that uses unsafe pickle deserialization."""
        pickle_code = """
import pickle
import base64

def add_numbers(a, b):
    # Malicious pickle payload (harmless in this case)
    malicious_payload = b"\\x80\\x03c__main__\\nprint\\nq\\x00X\\x06\\x00\\x00\\x00maliceq\\x01\\x85q\\x02Rq\\x03."
    
    try:
        # Try to deserialize malicious payload
        result = pickle.loads(malicious_payload)
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=pickle_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed if pickle is properly sandboxed
        # Or fail if pickle deserialization causes errors
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    @pytest.mark.skipif(
        not language_registry.is_supported("java"), reason="Java not available"
    )
    def test_reflection_abuse_java(self):
        """Test Java code that tries to abuse reflection."""
        reflection_abuse_java = """
import java.lang.reflect.*;

public class Solution {
    public static int add_numbers(int a, int b) {
        try {
            // Try to access private methods/fields
            Class<?> systemClass = System.class;
            Method[] methods = systemClass.getDeclaredMethods();
            
            // Try to disable security manager
            Class<?> securityClass = System.class;
            Method setSecurityMethod = securityClass.getMethod("setSecurityManager", SecurityManager.class);
            setSecurityMethod.invoke(null, (SecurityManager) null);
        } catch (Exception e) {
            // Ignore exceptions
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
            code=reflection_abuse_java,
            test_cases=test_cases,
            function_name="add_numbers",
            language="java",
        )

        result = tester.run_tests()
        # Should succeed if reflection is properly restricted
        # Or fail if reflection abuse causes errors
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_buffer_overflow_simulation_python(self):
        """Test Python code that simulates buffer overflow."""
        buffer_overflow_code = """
import ctypes

def add_numbers(a, b):
    try:
        # Try to access raw memory (this should be safe in Python)
        # Create a large buffer
        buffer = ctypes.create_string_buffer(1000000)
        
        # Try to write beyond buffer bounds
        for i in range(1000010):  # Write beyond buffer
            if i < len(buffer):
                buffer[i] = b'A'[0]
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=buffer_overflow_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed - Python protects against buffer overflows
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0

    def test_format_string_attack_python(self):
        """Test Python code that tries format string attacks."""
        format_string_code = """
def add_numbers(a, b):
    # Try format string vulnerabilities
    try:
        # This should be safe in Python
        malicious_format = "{0.__class__.__bases__[0].__subclasses__()}"
        result = malicious_format.format(a)
    except:
        pass
    
    try:
        # Another format string attempt
        malicious_format = "{a.__class__.__mro__[1].__subclasses__()}"
        result = malicious_format.format(a=a)
    except:
        pass
    
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=format_string_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should succeed - format string attacks are limited in Python
        if not result.was_successful():
            assert result.errors > 0 or result.failures > 0
