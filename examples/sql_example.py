import json
import os
import sys
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester

# Initialize code generator with OpenAI
code_generator = CodeGenerator(
    api_key=os.getenv("OPENAI_API_KEY"),
    client_type="openai"
)

#
# Example of standard Code Gen use
#

print("1. Standard Code Gen Example")
generated_code = code_generator.generate_code(
    "that selects all users from a users table where age is greater than a given value.",
    params="min_age",
    assumptions="min_age is an integer and users table has columns id, name, age",
    model="gpt-4o",
    language="sql"
)
print(f"Generated code:\n{generated_code}")

# Note: SQL testing would require a database connection, so we'll skip actual test execution
print("Note: SQL examples demonstrate code generation but skip test execution\n")


#
# Example of function redefinition use
#

print("2. Function Redefinition Example")
generated_code = code_generator.generate_code(
    "get_adult_users", 
    gen_type="redef", 
    params="min_age", 
    assumptions="min_age is an integer and users table has columns id, name, age",
    model="gpt-4o",
    language="sql"
)

print(f"Generated code:\n{generated_code}")
print("Note: SQL examples demonstrate code generation but skip test execution\n")

#
# Example of code generation with multiple generated functions
#

print("3. Multiple Function Generation Example")
generated_code = code_generator.generate_code(
    "that selects all users from a users table where age is greater than a given value.", 
    num_to_gen=5, 
    params="min_age", 
    assumptions="min_age is an integer and users table has columns id, name, age",
    model="gpt-4o",
    language="sql"
)

print(f"Generated code:\n{generated_code}")
print("Note: SQL examples demonstrate code generation but skip test execution\n")


#
# Example of code generation with multiple generated functions and redefinition
#

print("4. Multiple Function Generation with Redefinition Example")
generated_code = code_generator.generate_code(
    "filter_users_by_age", 
    gen_type="redef", 
    num_to_gen=5, 
    params="min_age", 
    assumptions="min_age is an integer and users table has columns id, name, age",
    model="gpt-4o",
    language="sql"
)
print(f"Generated code:\n{generated_code}")
print("Note: SQL examples demonstrate code generation but skip test execution\n")


#
# Example of code generation with segmentation
#

print("5. Segmentation Example")
generated_code = code_generator.generate_code(
    "joins users and orders tables, filters by user age, groups by user, and returns the count of orders per user.",
    segmentation_few_shot_file="segmentation_few_shot.json",
    params="min_age",
    assumptions="users table has columns id, name, age; orders table has columns id, user_id, amount",
    model="gpt-4o",
    language="sql"
)

print(f"Generated code:\n{generated_code['code']}")
print(f"Segmentation code:\n{json.dumps(generated_code['segmentation'], indent=4)}", end="\n\n")
print("Note: SQL examples demonstrate code generation but skip test execution\n")


#
# Example of code generation with segmentation on multiple functions
#

print("6. Segmentation with Multiple Function Generation Example")
generated_code = code_generator.generate_code(
    "joins users and orders tables, filters by user age, groups by user, and returns the count of orders per user.",
    segmentation_few_shot_file="segmentation_few_shot.json",
    num_to_gen=5,
    params="min_age",
    assumptions="users table has columns id, name, age; orders table has columns id, user_id, amount",
    model="gpt-4o",
    language="sql"
)

print(f"Generated code:\n{generated_code['code']}")
print(f"Segmentation code:\n{json.dumps(generated_code['segmentation'], indent=4)}", end="\n\n")
print("Note: SQL examples demonstrate code generation but skip test execution\n")