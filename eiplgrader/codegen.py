import json
import openai
import os

DEFAULT_SYSTEM_PROMPT_CGBG = """
Pretend you are an introductory CS student learning python for the
very first time. Your have a rudementary understanding of functions,
loops, variables, and conditionals.  Create a function, called foo,
according to the following prompt:

Create a function foo that {}

Include only the function and no additional test cases or code.
Respond with the code for the function foo in the following format:

```python
<code here>
```

"""

DEFAULT_SYSTEM_PROMPT_REDEF = """
Create a function based on the following function name: def {foo}({params}): pass
You are given the following assumptions about the arguments: {assumptions}.

Generate the code only and generate it to be surrounded with 
markdown. it is very important that you use the provided 
function name when generating the code. For example:

```python
def {foo}({params}):
    pass
```
"""


ALLOWED_MODELS_OPEN_AI = ["gpt-4o", "gpt-4", "gpt-4.5", "gpt-4.1"]

ALLOWED_MODELS_ANTHROPIC = []

ALLOWED_MODELS_META = []


class CodeGenerator:

    def __init__(self, api_key):

        self.client = openai.OpenAI(api_key=api_key)

    def generate_code(
        self,
        student_response,
        gen_type="cgbg",
        params=None,
        assumptions=None,
        num_to_gen=None,
        segmentation_few_shot_file=None,
        temperature=1,
        model="gpt-4o",
    ):

        # Validate temperature settings
        if temperature < 0 or temperature > 1:
            raise ValueError(
                f"Invalid temperature: {temperature}. Temperature must be between 0 and 1"
            )

        if temperature < 1:
            print(
                f"WARNING: Low temperature ({temperature}) may lead to"
                "non-deterministic responses. Doing this is discouraged as it"
                "may lead to non-deterministic grading."
            )

        self.num_to_gen = num_to_gen or 1

        # Determine which system prompt to use
        if gen_type == "redef":
            system_prompt = DEFAULT_SYSTEM_PROMPT_REDEF.format(
                foo=student_response,
                params=params if params else "",
                assumptions=assumptions if assumptions else "",
            )
        else:  # Default to cgbg
            system_prompt = DEFAULT_SYSTEM_PROMPT_CGBG.format(student_response)

        prompt = [{"role": "user", "content": system_prompt}]

        # Add segmentation few-shot examples if provided
        if segmentation_few_shot_file:

            # Check if it's an absolute path, otherwise look in examples directory
            if not os.path.isabs(segmentation_few_shot_file):
                examples_dir = os.path.join(
                    os.path.dirname(os.path.dirname(__file__)), "examples"
                )
                segmentation_few_shot_file = os.path.join(
                    examples_dir, segmentation_few_shot_file
                )

            try:
                with open(segmentation_few_shot_file, "r") as f:
                    segmentation_examples = json.load(f)

                # Add segmentation examples to the prompt
                prompt[0][
                    "content"
                ] += "\n\nHere are some examples of code segmentation:\n"
                prompt[0]["content"] += json.dumps(segmentation_examples, indent=2)
            except (FileNotFoundError, json.JSONDecodeError) as e:
                print(f"Error loading segmentation examples: {e}")

        allowed_models = ALLOWED_MODELS_OPEN_AI + ALLOWED_MODELS_ANTHROPIC
        if model not in allowed_models:
            raise ValueError(
                f"Invalid model: {model}. Allowed models are: {allowed_models}"
            )

        response = self.client.chat.completions.create(
            model=self.model,
            messages=prompt,
            temperature=self.temperature,
            n=self.num_to_gen,
        )

        if self.num_to_gen > 1:
            generated_code = [
                response.choices[i]
                .message.content.replace("```python", "")
                .replace("```", "")
                .strip()
                for i in range(self.num_to_gen)
            ]
        else:
            generated_code = (
                response.choices[0]
                .message.content.replace("```python", "")
                .replace("```", "")
                .strip()
            )

        return generated_code
