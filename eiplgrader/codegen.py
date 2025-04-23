import json
import os
import openai

DEFAULT_SYSTEM_PROMPT_CGBG = """
Pretend you are an introductory CS student learning python for the
very first time. Your have a rudementary understanding of functions,
loops, variables, and conditionals.  Create a function, called {function_name},
according to the following prompt:

Create a function {function_name} that {prompt}

Include only the function and no additional test cases or code.
Respond with the code for the function {function_name} in the following format:

```python
<code here>
```

"""

DEFAULT_SYSTEM_PROMPT_REDEF = """
Create a function based on the following function name: def {function_name}({params}):
pass. You are given the following assumptions about the arguments:
{assumptions}.

Generate the code only and generate it to be surrounded with
markdown. it is very important that you use the provided
function name when generating the code. For example:

```python
def {function_name}({params}):
    pass
```
"""

ALLOWED_MODELS_OPEN_AI = ["gpt-4o", "gpt-4", "gpt-4.5", "gpt-4.1"]
ALLOWED_MODELS_ANTHROPIC = []
ALLOWED_MODELS_META = []


class CodeGenerator:

    def __init__(self, api_key, client_type="openai"):
        self.api_key = api_key
        self.client_type = client_type
        self.model_request = None

        if client_type == "openai":
            self.client = openai.OpenAI(api_key=api_key)
        elif client_type == "anthropic":
            # Initialize anthropic client when implemented
            pass
        elif client_type == "meta":
            # Initialize meta client when implemented
            pass
        else:
            raise ValueError(
                f"Invalid client type: {client_type}. Allowed types are: openai, anthropic, meta"
            )

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
        function_name="foo",
    ):
        # Validate temperature
        if temperature < 0 or temperature > 1:
            raise ValueError(
                f"Invalid temperature: {temperature}. Temperature must be between 0 and 1"
            )

        if temperature < 1:
            print(
                f"WARNING: Low temperature ({temperature}) may lead to "
                "non-deterministic responses. Doing this is discouraged as it "
                "may lead to non-deterministic grading."
            )

        self._validate_model(model)

        self.model_request = self._create_model_request(
            model, temperature, num_to_gen or 1
        )

        prompt = self._create_prompt(
            student_response,
            gen_type,
            params,
            assumptions,
            segmentation_few_shot_file,
            function_name
        )

        return self.model_request.request(prompt)

    def _validate_model(self, model):
        """Validate that the model is supported by the client type."""
        allowed_models = []

        if self.client_type == "openai":
            allowed_models = ALLOWED_MODELS_OPEN_AI
        elif self.client_type == "anthropic":
            allowed_models = ALLOWED_MODELS_ANTHROPIC
        elif self.client_type == "meta":
            allowed_models = ALLOWED_MODELS_META

        if model not in allowed_models:
            raise ValueError(
                f"""Invalid model: {model}.
                Allowed models for {self.client_type} are: {allowed_models}"""
            )

    def _create_model_request(self, model, temperature, num_to_gen):
        """Create a ModelRequest object based on client type."""
        if self.client_type == "openai":
            return OpenAIModelRequest(self.client, model, temperature, num_to_gen)
        elif self.client_type == "anthropic":
            return AnthropicModelRequest(self.client, model, temperature, num_to_gen)
        elif self.client_type == "meta":
            return MetaModelRequest(self.client, model, temperature, num_to_gen)

    def _create_prompt(
        self,
        student_response,
        gen_type,
        params,
        assumptions,
        segmentation_few_shot_file,
        function_name="foo",
    ):
        """Create the prompt based on generation type and other parameters."""

        if gen_type == "redef":
            system_prompt = DEFAULT_SYSTEM_PROMPT_REDEF.format(
                function_name=student_response,
                params=params if params else "",
                assumptions=assumptions if assumptions else "",
            )
        else:
            system_prompt = DEFAULT_SYSTEM_PROMPT_CGBG.format(
                function_name=function_name,
                prompt=student_response
            )

        prompt = [{"role": "user", "content": system_prompt}]

        # Add segmentation examples if provided
        if segmentation_few_shot_file:
            self._add_segmentation_examples(prompt, segmentation_few_shot_file)

        return prompt

    def _add_segmentation_examples(self, prompt, segmentation_few_shot_file):
        """Add segmentation examples to the prompt if the file exists."""
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
            prompt[0]["content"] += "\n\nHere are some examples of code segmentation:\n"
            prompt[0]["content"] += json.dumps(segmentation_examples, indent=2)
        except (FileNotFoundError, json.JSONDecodeError) as e:
            print(f"Error loading segmentation examples: {e}")


class ModelRequest:
    def __init__(self, client, model, temperature, num_to_gen):
        self.client = client
        self.model = model
        self.temperature = temperature
        self.num_to_gen = num_to_gen

class OpenAIModelRequest(ModelRequest):

    def request(self, prompt):
        response = self.client.chat.completions.create(
            model=self.model,
            messages=prompt,
            temperature=self.temperature,
            n=self.num_to_gen,
        )

        return self._process_response(response)

    def _process_response(self, response):
        if self.num_to_gen > 1:
            return [
                response.choices[i]
                .message.content.replace("```python", "")
                .replace("```", "")
                .strip()
                for i in range(self.num_to_gen)
            ]
        else:
            return (
                response.choices[0]
                .message.content.replace("```python", "")
                .replace("```", "")
                .strip()
            )


class AnthropicModelRequest(ModelRequest):

    def request(self, prompt):
        raise NotImplementedError("Anthropic API support not yet implemented")

    def _process_response(self, response):
        pass


class MetaModelRequest(ModelRequest):

    def request(self, prompt):
        raise NotImplementedError("Meta API support not yet implemented")

    def _process_response(self, response):
        pass
