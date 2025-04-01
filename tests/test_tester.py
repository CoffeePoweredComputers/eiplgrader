import unittest
from eiplgrader.tester import CodeTester


class TestCodeTester(unittest.TestCase):

    def test_run_tests(self):
        generated_code = """
def add(a, b):
    return a + b
"""
        test_cases = [
            ["class TestAddFunction(unittest.TestCase):",
             "    def test_add(self):",
             "        self.assertEqual(add(1, 2), 3)"]
        ]
        tester = CodeTester(generated_code, test_cases)
        result = tester.run_tests()
        self.assertTrue(result.wasSuccessful())


if __name__ == '__main__':
    unittest.main()
