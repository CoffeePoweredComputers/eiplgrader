import json
import os
from typing import List, Dict, Any, Optional

import openai

DEFAULT_STUDENT_MODEL = """
Pretend you are an introductory CS student learning python for the very first
time. You have a rudimentary understanding of functions, loops, variables, and
conditionals. You also don't know about type annotations.
"""

DEFAULT_SYSTEM_PROMPT_CGBG = """
Create a function, called {function_name},
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

DEFAULT_SYSTEM_PROMPT_ROBUSTNESS = """
Generate {num_to_gen} different versions of this function with these formatting
constraints.
"""

DEFAULT_SEGMENTATION_RESPONSE_FORMAT = {
    "type": "json_schema",
    "json_schema": {
        "name": "group_list",
        "schema": {
            "type": "object",
            "required": ["groups"],
            "properties": {
                "groups": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "required": ["explanation_portion", "code"],
                        "properties": {
                            "code": {
                                "type": "string",
                                "description": "A string that contains the actual code related to the explanation.",
                            },
                            "explanation_portion": {
                                "type": "string",
                                "description": "A string that explains the code's content or purpose.",
                            },
                        },
                        "additionalProperties": False,
                    },
                    "description": "Groups of explanations with their corresponding code.",
                }
            },
            "additionalProperties": False,
        },
        "strict": True,
    },
}

ALLOWED_CLIENTS = [
    "openai",
    "anthropic",
    "meta",
]


ALLOWED_MODELS_OPEN_AI: List[str] = ["gpt-4o", "gpt-4", "gpt-4.5", "gpt-4.1"]
ALLOWED_MODELS_ANTHROPIC: List[str] = []
ALLOWED_MODELS_META: List[str] = []


class CodeGenerator:

    def __init__(self, api_key: str, client_type: str = "openai"):
        self.api_key: str = api_key
        self.client_type: str = client_type
        self.model_request: Optional["ModelRequest"] = None

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
                f"Invalid client type: {client_type}. Allowed types are: {ALLOWED_CLIENTS}"
            )

    def generate_code(
        self,
        student_response: str,
        gen_type: str = "cgbg",
        params: str = "",
        assumptions: str = "",
        num_to_gen: int = 1,
        segmentation_few_shot_file: str = "",
        temperature: float = 1.0,
        model: str = "gpt-4o",
        function_name: str = "foo",
    ) -> Dict[str, Any]:

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

        self.model_request = self._create_model_request(model, temperature, num_to_gen)

        prompt = self._create_prompt(
            student_response,
            gen_type,
            params,
            assumptions,
            function_name,
            num_to_gen,
        )

        if self.model_request is None or not isinstance(
            self.model_request, ModelRequest
        ):
            raise ValueError("ModelRequest is not initialized correctly.")

        code_response = self.model_request.request_function_generation(prompt)

        if gen_type != "cgbg" and segmentation_few_shot_file:
            raise ValueError(
                f"Segmentation is not supported for generation type '{gen_type}'."
            )

        if not segmentation_few_shot_file:
            return {"code": code_response}

        segmentation_results = self._run_segmentation(
            student_response, code_response, segmentation_few_shot_file
        )

        result = {"code": code_response, "segmentation": segmentation_results}
        return result

    def _validate_model(self, model: str) -> None:
        """Validate that the model is supported by the client type."""
        allowed_models = []

        if self.client_type == "openai":
            allowed_models = ALLOWED_MODELS_OPEN_AI
        elif self.client_type == "anthropic":
            allowed_models = ALLOWED_MODELS_ANTHROPIC
        elif self.client_type == "meta":
            allowed_models = ALLOWED_MODELS_META

        if model not in allowed_models:
            model_list = ", ".join(allowed_models)
            raise ValueError(
                f"Invalid model: {model}. Allowed models for {self.client_type} are: {model_list}"
            )

    def _create_model_request(self, model: str, temperature: float, num_to_gen: int):
        """Create a ModelRequest object based on client type."""

        if self.client_type == "openai":
            return OpenAIModelRequest(self.client, model, temperature, num_to_gen)

        if self.client_type == "anthropic":
            return AnthropicModelRequest(self.client, model, temperature, num_to_gen)

        if self.client_type == "meta":
            return MetaModelRequest(self.client, model, temperature, num_to_gen)

        raise ValueError(
            f"Invalid client type: {self.client_type}. Allowed types are: openai, anthropic, meta"
        )

    def _create_prompt(
        self,
        student_response: str,
        gen_type: str,
        params: str,
        assumptions: str,
        function_name: str = "foo",
        num_to_gen: int = 1,
    ) -> str:
        """Create the prompt based on generation type and other parameters."""

        if gen_type not in ["cgbg", "redef"]:
            raise ValueError(
                f"Invalid gen_type: {gen_type}. Allowed types are: 'cgbg', 'redef'"
            )

        system_prompt = DEFAULT_STUDENT_MODEL

        if gen_type == "redef":
            system_prompt += DEFAULT_SYSTEM_PROMPT_REDEF.format(
                function_name=student_response,
                params=params if params else "",
                assumptions=assumptions if assumptions else "",
            )
        else:
            system_prompt += DEFAULT_SYSTEM_PROMPT_CGBG.format(
                function_name=function_name, prompt=student_response
            )

        if num_to_gen > 1:
            system_prompt += DEFAULT_SYSTEM_PROMPT_ROBUSTNESS.format(
                num_to_gen=num_to_gen
            )

        return system_prompt

    def _run_segmentation(
        self,
        student_response: str,
        generated_functions: List[str],
        segmentation_few_shot_file: str,
    ) -> List[Dict[str, Any]]:
        """Runs segmentation on the generated code using few-shot examples."""

        segmentation_examples = self._load_segmentation_examples(
            segmentation_few_shot_file
        )

        if self.model_request is None or not isinstance(
            self.model_request, ModelRequest
        ):
            raise ValueError("ModelRequest is not initialized correctly.")

        if not segmentation_examples:
            return []

        try:
            segmentation_request = self._create_model_request(
                self.model_request.model, 0, 1
            )

            segmentation_results = list(
                map(
                    lambda x: segmentation_request.request_segmentation(
                        student_response, x, segmentation_examples
                    ),
                    generated_functions,
                )
            )

            return segmentation_results

        except (ValueError, TypeError, KeyError, json.JSONDecodeError) as e:
            print(f"Error during segmentation: {e}")
            return []

    def _load_segmentation_examples(
        self, segmentation_few_shot_file: str
    ) -> Dict[str, Any]:
        """Loads and validates segmentation examples from a file."""

        if not os.path.isabs(segmentation_few_shot_file):

            examples_dir = os.path.join(
                os.path.dirname(os.path.dirname(__file__)), "examples"
            )
            segmentation_few_shot_file = os.path.join(
                examples_dir, segmentation_few_shot_file
            )

        try:
            with open(segmentation_few_shot_file, "r", encoding="utf-8") as f:
                segmentation_examples = json.load(f)
        except (FileNotFoundError, json.JSONDecodeError) as e:
            print(f"Error loading segmentation examples: {e}")
            return {}

        if not isinstance(segmentation_examples, dict):
            raise ValueError(
                f"Invalid segmentation examples format: {segmentation_few_shot_file}. "
                "Expected a dictionary."
            )

        for required_attribute in ["multistructural", "relational"]:
            if required_attribute not in segmentation_examples:
                raise ValueError(
                    f"Missing required attribute '{required_attribute}' in segmentation examples."
                )
            for required_sub_attribute in ["description", "segmentation"]:
                if (
                    required_sub_attribute
                    not in segmentation_examples[required_attribute]
                ):
                    raise ValueError(
                        f"Missing required attribute '{required_sub_attribute}' in segmentation examples."
                    )

        return segmentation_examples


class ModelRequest:
    """
    Base class for model API request implementations.

    This class defines the interface for making requests to different model APIs
    and provides common properties needed by all model request implementations.
    """

    def __init__(self, client, model: str, temperature: float, num_to_gen: int):
        """Initialize a ModelRequest.

        Args:
            client: The API client instance
            model: The model name/identifier to use
            temperature: The temperature setting for generation (0.0-1.0)
            num_to_gen: Number of responses to generate
        """
        self.client = client
        self.model = model
        self.temperature = temperature
        self.num_to_gen = num_to_gen

    def request_function_generation(self, prompt: str) -> List[str]:
        """Make a request to the model API for function generation."""

        raise NotImplementedError(
            "Subclasses must implement request_function_generation method"
        )

    def request_segmentation(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Make a request to the model API for code segmentation."""

        raise NotImplementedError(
            "Subclasses must implement request_segmentation method"
        )


class OpenAIModelRequest(ModelRequest):
    """
    Model request implementation for OpenAI API.
    Handles making requests to OpenAI models and processing their responses.
    """

    def request_function_generation(self, prompt: str) -> List[str]:
        """Make a request to the OpenAI API for function generation."""

        formatted_prompt = [{"role": "user", "content": prompt}]
        response = self.client.chat.completions.create(
            model=self.model,
            messages=formatted_prompt,
            temperature=self.temperature,
            n=self.num_to_gen,
        )

        return self._process_function_response(response)

    def _process_function_response(self, response) -> List[str]:
        """Process the function generation response from OpenAI API."""

        response = response.choices[0].message.content

        functions = list(
            map(lambda x: x.split("```")[0].strip(), response.split("```python"))
        )[1:]

        return functions

    def request_segmentation(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Make a request to the OpenAI API for code segmentation."""

        segmentation_messages = self._format_segmentation_messages(
            student_response, code, segmentation_examples
        )

        segmentation_response = self.client.chat.completions.create(
            model=self.model,
            messages=segmentation_messages,
            temperature=0,  # Use temperature 0 for deterministic results
            response_format=DEFAULT_SEGMENTATION_RESPONSE_FORMAT,
            max_tokens=2048,
        )

        return self._process_segmentation_response(segmentation_response)

    def _format_segmentation_messages(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> List[Dict[str, str]]:
        """Format messages for segmentation request."""

        task_description = (
            "Task: Create a one-to-one mapping between each "
            "segment of a given explanation and the group of lines in the "
            "given code which that phrase is associated with. Not all of "
            "the description needs to be used. Not all of the code needs to "
            "be used. It's very important to only use the words in the "
            "user's provided explanation. One segment can map to multiple "
            f"lines.\n\nHere is the code:\n\n{code}"
        )
        segmentation_messages = [
            {
                "role": "system",
                "content": task_description,
            }
        ]

        segmentation_messages.append(
            {
                "role": "user",
                "content": segmentation_examples["multistructural"]["description"],
            }
        )

        segmentation_messages.append(
            {
                "role": "assistant",
                "content": json.dumps(
                    {"groups": segmentation_examples["multistructural"]}
                ),
            }
        )

        segmentation_messages.append({"role": "user", "content": ""})
        segmentation_messages.append(
            {
                "role": "assistant",
                "content": json.dumps({"groups": segmentation_examples["relational"]}),
            }
        )

        segmentation_messages.append({"role": "user", "content": student_response})

        return segmentation_messages

    def _process_segmentation_response(self, response) -> Dict[str, Any]:
        """Process the segmentation response from OpenAI API."""

        segmentation_content = response.choices[0].message.content
        segmentation_results = json.loads(segmentation_content)

        if not isinstance(segmentation_results, dict):
            raise ValueError(
                f"Invalid segmentation response format: {segmentation_content}"
            )

        return segmentation_results


class AnthropicModelRequest(ModelRequest):
    """Model request implementation for Anthropic API.

    This is a placeholder for future implementation of Anthropic API support.
    """

    def request_function_generation(self, prompt: str) -> List[str]:
        """Make a request to the Anthropic API for function generation (not yet implemented)."""
        raise NotImplementedError("Anthropic API support not yet implemented")

    def request_segmentation(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Make a request to the Anthropic API for code segmentation (not yet implemented)."""
        raise NotImplementedError("Anthropic API support not yet implemented")


class MetaModelRequest(ModelRequest):
    """Model request implementation for Meta API.

    This is a placeholder for future implementation of Meta API support.
    """

    def request_function_generation(self, prompt: str) -> List[str]:
        """Make a request to the Meta API for function generation (not yet implemented)."""
        raise NotImplementedError("Meta API support not yet implemented")

    def request_segmentation(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Make a request to the Meta API for code segmentation (not yet implemented)."""
        raise NotImplementedError("Meta API support not yet implemented")
