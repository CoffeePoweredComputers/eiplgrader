import unittest
from eiplgrader.tester import CodeTester, CodeTestResult

class TestCodeTester(unittest.TestCase):

    def test_simple_function(self):
        code = """
def foo(a, b):
    return a + b
"""
        test_cases = [
            {"parameters": {"a": 1, "b": 2}, "expected": 3},
            {"parameters": {"a": -1, "b": 5}, "expected": 4},
        ]
        tester = CodeTester(code, test_cases, function_name="foo")
        result = tester.run_tests()
        self.assertIsInstance(result, CodeTestResult)
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(len(result.test_results), 2)
        for tr in result.test_results:
            self.assertTrue(tr["pass"])
            self.assertEqual(tr["actual_output"], tr["expected_output"])

    def test_inplace_modification(self):
        code = """
def foo(lst):
    lst.append(4)
"""
        input_list = [1, 2, 3]
        test_cases = [
            {"parameters": {"lst": input_list}, "expected": [1, 2, 3, 4]},
        ]
        tester = CodeTester(code, test_cases, inplace="1", function_name="foo")
        result = tester.run_tests()
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.test_results[0]["actual_output"], [1, 2, 3, 4])

    def test_list_of_codes(self):
        codes = [
            """
def foo(a, b):
    return a + b
""",
            """
def foo(a, b):
    return a * b
"""
        ]
        test_cases = [
            {"parameters": {"a": 2, "b": 3}, "expected": 5},
            {"parameters": {"a": 4, "b": 5}, "expected": 20}
        ]
        tester = CodeTester(codes, test_cases, function_name="foo")
        results = tester.run_tests()
        self.assertIsInstance(results, list)
        self.assertEqual(len(results), 2)
        self.assertTrue(results[0].wasSuccessful())
        self.assertTrue(results[1].wasSuccessful())

    def test_function_not_found(self):
        code = """
def bar(a):
    return a
"""
        test_cases = [{"parameters": {"a": 1}, "expected": 1}]
        tester = CodeTester(code, test_cases, function_name="foo")
        with self.assertRaises(AttributeError):
            tester.run_tests()

    def test_invalid_test_case_structure(self):
        code = """
def foo(a):
    return a
"""
        # Non-dict test case
        test_cases = ["not a dict"]
        tester = CodeTester(code, test_cases, function_name="foo")
        with self.assertRaises(ValueError):
            tester.run_tests()
        # Missing 'parameters'
        test_cases = [{"expected": 1}]
        tester = CodeTester(code, test_cases, function_name="foo")
        with self.assertRaises(ValueError):
            tester.run_tests()
        # Missing 'expected'
        test_cases = [{"parameters": {"a": 1}}]
        tester = CodeTester(code, test_cases, function_name="foo")
        with self.assertRaises(ValueError):
            tester.run_tests()

if __name__ == "__main__":
    unittest.main()
