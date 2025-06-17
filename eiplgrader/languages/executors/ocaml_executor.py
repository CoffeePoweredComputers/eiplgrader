"""OCaml language executor for code testing."""

import os
import json
import tempfile
from typing import Dict, Any, Tuple, List
from .base_executors import CompiledLanguageExecutor


class OcamlExecutor(CompiledLanguageExecutor):
    """Executor for OCaml language code testing."""

    def __init__(self):
        super().__init__(compile_cmd=["ocamlc"], run_cmd=["./a.out"], file_ext=".ml")

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare OCaml code for execution with test harness."""
        function_name = test_case.get("function_name", "foo")
        parameters = test_case.get("parameters", {})
        inplace_mode = test_case.get("inplace", "0")

        # OCaml test harness with JSON support using Yojson-style parsing
        json_parser = """
(* Simple JSON parsing helpers *)
let rec parse_json_list parser s =
  let s = String.trim s in
  if String.length s < 2 || s.[0] <> '[' || s.[String.length s - 1] <> ']' then
    []
  else
    let content = String.sub s 1 (String.length s - 2) in
    let rec split_elements str acc current in_string =
      match str with
      | "" -> 
        let trimmed = String.trim current in
        if trimmed = "" then List.rev acc
        else List.rev (parser trimmed :: acc)
      | _ ->
        let c = str.[0] in
        let rest = String.sub str 1 (String.length str - 1) in
        match c with
        | '"' -> split_elements rest acc (current ^ String.make 1 c) (not in_string)
        | ',' when not in_string -> 
          let trimmed = String.trim current in
          if trimmed = "" then split_elements rest acc "" false
          else split_elements rest (parser trimmed :: acc) "" false
        | _ -> split_elements rest acc (current ^ String.make 1 c) in_string
    in
    split_elements content [] "" false

let parse_int_list s = parse_json_list int_of_string s

let parse_string_list s = 
  parse_json_list (fun s ->
    let s = String.trim s in
    if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"' then
      String.sub s 1 (String.length s - 2)
    else
      s
  ) s

let parse_float_list s = parse_json_list float_of_string s

let rec parse_json_object s =
  let s = String.trim s in
  if String.length s < 2 || s.[0] <> '{' || s.[String.length s - 1] <> '}' then
    []
  else
    let content = String.sub s 1 (String.length s - 2) in
    let rec split_pairs str acc current key in_string level =
      match str with
      | "" -> 
        if key <> "" && current <> "" then
          List.rev ((key, String.trim current) :: acc)
        else
          List.rev acc
      | _ ->
        let c = str.[0] in
        let rest = String.sub str 1 (String.length str - 1) in
        match c with
        | '"' -> split_pairs rest acc (current ^ String.make 1 c) key (not in_string) level
        | ':' when not in_string && key = "" -> 
          let k = String.trim current in
          let k = if String.length k >= 2 && k.[0] = '"' && k.[String.length k - 1] = '"' then
            String.sub k 1 (String.length k - 2)
          else k in
          split_pairs rest acc "" k false level
        | ',' when not in_string && level = 0 -> 
          let acc = if key <> "" then (key, String.trim current) :: acc else acc in
          split_pairs rest acc "" "" false 0
        | '[' when not in_string -> split_pairs rest acc (current ^ String.make 1 c) key false (level + 1)
        | ']' when not in_string -> split_pairs rest acc (current ^ String.make 1 c) key false (level - 1)
        | '{' when not in_string -> split_pairs rest acc (current ^ String.make 1 c) key false (level + 1)
        | '}' when not in_string -> split_pairs rest acc (current ^ String.make 1 c) key false (level - 1)
        | _ -> split_pairs rest acc (current ^ String.make 1 c) key in_string level
    in
    split_pairs content [] "" "" false 0

let get_json_field obj field =
  try List.assoc field obj
  with Not_found -> ""

(* JSON output helpers *)
let json_of_string s = "\"" ^ s ^ "\""

let json_of_int n = string_of_int n

let json_of_float f = string_of_float f

let json_of_bool b = if b then "true" else "false"

let rec json_of_list f lst =
  "[" ^ String.concat "," (List.map f lst) ^ "]"

let json_of_int_list = json_of_list json_of_int
let json_of_string_list = json_of_list json_of_string
let json_of_float_list = json_of_list json_of_float

(* Helper to read all of stdin *)
let read_all_stdin () =
  let rec read_lines acc =
    try
      let line = read_line () in
      read_lines (line :: acc)
    with End_of_file ->
      List.rev acc
  in
  String.concat "\n" (read_lines [])
"""

        # Generate parameter extraction code
        param_extraction = ""
        param_names = list(parameters.keys())

        if parameters:
            param_extraction = "  (* Parse JSON parameters *)\n"
            param_extraction += "  let json_obj = parse_json_object input in\n"

            for name, value in parameters.items():
                if isinstance(value, bool):
                    param_extraction += f'  let {name} = (get_json_field json_obj "{name}") = "true" in\n'
                elif isinstance(value, int):
                    param_extraction += f'  let {name} = int_of_string (get_json_field json_obj "{name}") in\n'
                elif isinstance(value, float):
                    param_extraction += f'  let {name} = float_of_string (get_json_field json_obj "{name}") in\n'
                elif isinstance(value, str):
                    param_extraction += (
                        f'  let {name} = get_json_field json_obj "{name}" in\n'
                    )
                elif isinstance(value, list):
                    if not value:
                        param_extraction += f"  let {name} = [] in\n"
                    elif isinstance(value[0], int):
                        param_extraction += f'  let {name} = parse_int_list (get_json_field json_obj "{name}") in\n'
                    elif isinstance(value[0], str):
                        param_extraction += f'  let {name} = parse_string_list (get_json_field json_obj "{name}") in\n'
                    elif isinstance(value[0], float):
                        param_extraction += f'  let {name} = parse_float_list (get_json_field json_obj "{name}") in\n'

        # Generate function call based on inplace mode
        function_call = ""  # Initialize to ensure it's always defined

        if inplace_mode == "0":
            # Normal function call - function returns a value
            if param_names:
                function_call = (
                    f"  let result = {function_name} {' '.join(param_names)} in\n"
                )
            else:
                function_call = f"  let result = {function_name} () in\n"

            # Determine output format based on expected result type
            output_code = self._generate_output_code(test_case.get("expected"))

        elif inplace_mode == "1":
            # Function modifies arguments in-place (rare in OCaml, usually via refs)
            if param_names:
                first_param = param_names[0]
                other_params = " ".join(param_names[1:]) if len(param_names) > 1 else ""

                # Assume first parameter is a ref type
                param_extraction = param_extraction.replace(
                    f"let {first_param} =", f"let {first_param}_val ="
                )
                param_extraction += f"  let {first_param} = ref {first_param}_val in\n"

                if other_params:
                    function_call = (
                        f"  let () = {function_name} {first_param} {other_params} in\n"
                    )
                else:
                    function_call = f"  let () = {function_name} {first_param} in\n"

                # Output the dereferenced value
                output_code = f"  (* Output modified parameter *)\n"
                output_code += f"  print_string (json_of_int !{first_param});\n"
                output_code += "  print_newline ()\n"
            else:
                function_call = f"  let () = {function_name} () in\n"
                output_code = '  print_string "null";\n  print_newline ()\n'

        elif inplace_mode == "2":
            # Function both modifies in-place and returns a value
            if param_names:
                first_param = param_names[0]
                other_params = " ".join(param_names[1:]) if len(param_names) > 1 else ""

                # Assume first parameter is a ref type
                param_extraction = param_extraction.replace(
                    f"let {first_param} =", f"let {first_param}_val ="
                )
                param_extraction += f"  let {first_param} = ref {first_param}_val in\n"

                if other_params:
                    function_call = f"  let result = {function_name} {first_param} {other_params} in\n"
                else:
                    function_call = f"  let result = {function_name} {first_param} in\n"

                output_code = self._generate_output_code(test_case.get("expected"))
            else:
                function_call = f"  let result = {function_name} () in\n"
                output_code = self._generate_output_code(test_case.get("expected"))

        else:
            # Default case for unexpected inplace_mode values - treat as normal function call
            if param_names:
                function_call = (
                    f"  let result = {function_name} {' '.join(param_names)} in\n"
                )
            else:
                function_call = f"  let result = {function_name} () in\n"
            output_code = self._generate_output_code(test_case.get("expected"))

        # Build the main function
        main_code = f"""
(* Main test harness *)
let () =
  (* Read JSON input from stdin *)
  let input = read_all_stdin () in
{param_extraction}
  (* Call the function *)
{function_call}
{output_code}
"""

        # Combine everything
        return json_parser + "\n\n" + code + "\n\n" + main_code

    def _generate_output_code(self, expected_value):
        """Generate appropriate output code based on expected value type."""
        if expected_value is None:
            return '  print_string "null";\n  print_newline ()'
        elif isinstance(expected_value, bool):
            return "  print_string (json_of_bool result);\n  print_newline ()"
        elif isinstance(expected_value, int):
            return "  print_string (json_of_int result);\n  print_newline ()"
        elif isinstance(expected_value, float):
            return "  print_string (json_of_float result);\n  print_newline ()"
        elif isinstance(expected_value, str):
            return "  print_string (json_of_string result);\n  print_newline ()"
        elif isinstance(expected_value, list):
            if not expected_value:
                return '  print_string "[]";\n  print_newline ()'
            elif isinstance(expected_value[0], int):
                return "  print_string (json_of_int_list result);\n  print_newline ()"
            elif isinstance(expected_value[0], str):
                return (
                    "  print_string (json_of_string_list result);\n  print_newline ()"
                )
            elif isinstance(expected_value[0], float):
                return "  print_string (json_of_float_list result);\n  print_newline ()"
            else:
                # Default to string representation
                return "  print_string (json_of_string (string_of_int result));\n  print_newline ()"
        else:
            # Default case
            return "  print_string (json_of_string (string_of_int result));\n  print_newline ()"

    def compile(self, code_path: str) -> Tuple[bool, str, str]:
        """Compile OCaml code."""
        output_path = code_path.replace(self.file_ext, "")
        cmd = self.compile_cmd + ["-o", output_path, code_path]

        import subprocess

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return (True, output_path, "")
        else:
            return (False, output_path, result.stderr)

    def cleanup(self) -> None:
        """Clean up temporary directory and OCaml compilation artifacts."""
        if os.path.exists(self.temp_dir):
            # Also clean up .cmi and .cmo files
            for root, dirs, files in os.walk(self.temp_dir):
                for file in files:
                    if file.endswith((".cmi", ".cmo")):
                        os.unlink(os.path.join(root, file))

        # Call parent cleanup
        super().cleanup()
