"""Ruby language executor for code testing."""

import json
import os
from typing import Dict, Any
from copy import deepcopy
from ..executors.base_executors import InterpretedLanguageExecutor


class RubyExecutor(InterpretedLanguageExecutor):
    """Executor for Ruby language code testing."""

    def __init__(self):
        super().__init__(interpreter_cmd=["ruby"], file_ext=".rb")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Ruby code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        expected = test_case.get("expected")
        inplace_mode = test_case.get("inplace", "0")

        # Create the test harness
        test_harness = f"""
require 'json'
require 'pp'

# Generated function
{code}

# Test execution
begin
  # Prepare arguments
  params = {json.dumps(parameters)}
  args = params.values
  
  # Handle different inplace modes
  inplace_mode = "{inplace_mode}"
  
  case inplace_mode
  when "0"
    # Normal function call - function returns a value
    actual = {function_name}(*args)
  when "1"
    # Function modifies arguments in-place
    if args.any?
      actual = Marshal.load(Marshal.dump(args[0]))  # Deep copy
      {function_name}(actual, *args[1..-1])
    else
      actual = {function_name}()
    end
  when "2"
    # Function both modifies in-place and returns a value
    if args.any?
      modified_arg = Marshal.load(Marshal.dump(args[0]))  # Deep copy
      result = {function_name}(modified_arg, *args[1..-1])
      # Return the result if not nil, otherwise the modified argument
      actual = result.nil? ? modified_arg : result
    else
      actual = {function_name}()
    end
  else
    raise "Invalid inplace mode: #{{inplace_mode}}"
  end
  
  # Output result as JSON
  puts JSON.generate(actual)
  
rescue Exception => e
  # Output error information
  error_info = {{
    "error" => e.message,
    "backtrace" => e.backtrace[0..2]
  }}
  STDERR.puts JSON.generate(error_info)
  exit 1
end
"""
        return test_harness
