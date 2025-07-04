import json
import os
from typing import List, Dict, Any, Optional

import openai
import requests
from .languages import language_registry

DEFAULT_RETURN_TYPE = """
Unless otherwise specified by the user or in the function name, the function should always
return rather than print values.
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
                                "description": (
                                    "A string that contains the actual code related to the explanation."
                                ),
                            },
                            "explanation_portion": {
                                "type": "string",
                                "description": (
                                    "A string that explains the code's content or purpose."
                                ),
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

ALLOWED_CLIENTS = ["openai", "anthropic", "meta", "ollama"]


ALLOWED_MODELS_OPEN_AI: List[str] = ["gpt-4o", "gpt-4", "gpt-4.5", "gpt-4.1"]
ALLOWED_MODELS_ANTHROPIC: List[str] = []
ALLOWED_MODELS_META: List[str] = [
    "Llama-4-Maverick-17B-128E-Instruct-FP8",
    "Llama-4-Scout-17B-16E-Instruct-FP8",
    "Llama-3.3-70B-Instruct",
    "Llama-3.3-8B-Instruct",
]
# ALLOWED_MODELS_OLLAMA: List[str] = [ "llama3.1", "llama3.2", "llama3.2:1b", "deepseek-r1", "codellama" ]
ALLOWED_MODELS_OLLAMA: List[str] = ["codellama:instruct", "stable-code:instruct"]


class CodeGenerator:

    def __init__(
        self,
        api_key: str,
        client_type: str,
        ollama_base_url: str = "http://localhost:11434",
        language: str = "python",
    ) -> None:
        self.api_key: str = api_key
        self.client_type: str = client_type
        self.model_request: Optional["ModelRequest"] = None
        self.ollama_base_url: str = ollama_base_url
        self.language: str = language
        self.client: Optional[openai.OpenAI] = None

        if client_type == "openai":
            self.client = openai.OpenAI(api_key=api_key)
        elif client_type == "anthropic":
            # Initialize anthropic client when implemented
            pass
        elif client_type == "meta":
            # Initialize meta client when implemented
            pass
        elif client_type == "ollama":
            self.client = None
        else:
            raise ValueError(
                f"Invalid client type: {client_type}. Allowed types are: {ALLOWED_CLIENTS}"
            )

    def generate_code(
        self,
        student_response: str,
        model: str,
        gen_type: str = "cgbg",
        params: str = "",
        assumptions: str = "",
        num_to_gen: int = 1,
        segmentation_few_shot_file: str = "",
        temperature: float = 1.0,
        function_name: str = "foo",
        language: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Generate code based on the student's response using the specified model and parameters.
        Args:
            student_response (str): The student's response to generate code from.
            model (str): The model to use for code generation.
            gen_type (str): The type of generation ('cgbg' for code generation by generation,
                            'redef' for redefining a function).
            params (str): Additional parameters for the function.
            assumptions (str): Assumptions made in the function.
            num_to_gen (int): Number of code variations to generate.
            segmentation_few_shot_file (str): Path to the segmentation few-shot examples file.
            temperature (float): Temperature for the model response (0.0-1.0).
            function_name (str): Name of the function to generate.
            language (Optional[str]): Programming language for code generation.
        Returns:
            Dict[str, Any]: A dictionary containing the generated code and language.
        Raises:
            ValueError: If the model, client type, or generation type is invalid.
        """

        if gen_type not in ["cgbg", "redef"]:
            raise ValueError(
                f"Invalid gen_type: {gen_type}. Allowed types are: 'cgbg', 'redef'"
            )

        if temperature < 0 or temperature > 1:
            raise ValueError(
                f"Invalid temperature: {temperature}. Temperature must be between 0 and 1"
            )

        if temperature > 0:
            print(
                f"WARNING: Temperatures higher than 0 ({temperature}) may lead to "
                "non-deterministic responses. Doing this is discouraged as it "
                "may lead to non-deterministic grading."
            )

        # Use language parameter or instance default
        lang = language or self.language
        adapter = language_registry.get_adapter(lang)

        if not adapter:
            raise ValueError(f"Unsupported language: {lang}")

        self._validate_model(model)

        self.model_request = self._create_model_request(model, temperature, num_to_gen)

        # NOTE: if the student is defining the function then we use the student
        # response as the function name
        if gen_type == "redef":
            function_name = student_response

        # Use language adapter to create prompt
        prompt = adapter.generate_prompt(
            student_response=student_response,
            function_name=function_name,
            gen_type=gen_type,
            params=params,
            assumptions=assumptions,
            num_to_gen=num_to_gen,
        )

        if num_to_gen > 1:
            prompt += DEFAULT_SYSTEM_PROMPT_ROBUSTNESS.format(num_to_gen=num_to_gen)

        prompt += DEFAULT_RETURN_TYPE

        if self.model_request is None or not isinstance(
            self.model_request, ModelRequest
        ):
            raise ValueError("ModelRequest is not initialized correctly.")

        raw_response = self.model_request.request_function_generation(prompt)

        # Extract code using language adapter
        code_blocks = adapter.extract_code(raw_response)

        # Normalize whitespace and remove any comments that might have leaked through
        code_blocks = [adapter.normalize_code(code) for code in code_blocks]

        if gen_type != "cgbg" and segmentation_few_shot_file:
            raise ValueError(
                f"Segmentation is not supported for generation type '{gen_type}'."
            )

        if not segmentation_few_shot_file:
            return {"code": code_blocks, "language": lang}

        segmentation_results = self._run_segmentation(
            student_response, code_blocks, segmentation_few_shot_file
        )

        result = {
            "code": code_blocks,
            "segmentation": segmentation_results,
            "language": lang,
        }
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
        elif self.client_type == "ollama":
            allowed_models = ALLOWED_MODELS_OLLAMA

        if model not in allowed_models:
            model_list = ", ".join(allowed_models)
            raise ValueError(
                f"Invalid model: {model}. Allowed models for {self.client_type} are: {model_list}"
            )

    def _create_model_request(
        self, model: str, temperature: float, num_to_gen: int
    ) -> "ModelRequest":
        """Create a ModelRequest object based on client type."""

        if self.client_type == "openai":
            return OpenAIModelRequest(self.client, model, temperature, num_to_gen)

        if self.client_type == "anthropic":
            return AnthropicModelRequest(self.client, model, temperature, num_to_gen)

        if self.client_type == "meta":
            return LlamaRequest(self.api_key, model, temperature, num_to_gen)

        if self.client_type == "ollama":
            return OllamaModelRequest(
                self.ollama_base_url, model, temperature, num_to_gen
            )

        raise ValueError(
            f"Invalid client type: {self.client_type}. Allowed types are: openai, anthropic, meta, ollama"
        )

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

            segmentation_results = [
                segmentation_request.request_segmentation(
                    student_response, code, segmentation_examples
                )
                for code in generated_functions
            ]

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

    def __init__(
        self, client: Any, model: str, temperature: float, num_to_gen: int
    ) -> None:
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

    def request_function_generation(self, prompt: str) -> str:
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

    def request_function_generation(self, prompt: str) -> str:
        """Make a request to the OpenAI API for function generation."""

        formatted_prompt = [{"role": "user", "content": prompt}]
        response = self.client.chat.completions.create(
            model=self.model,
            messages=formatted_prompt,
            temperature=self.temperature,
            n=self.num_to_gen,
        )

        content = response.choices[0].message.content
        if content is None or not isinstance(content, str):
            raise RuntimeError("API returned empty content")
        # At this point, content is guaranteed to be a str
        result: str = content
        return result

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

        task_description = f"""
        # Task: 
        Map EXACT portions of the student's explanation to corresponding code sections.

        # CRITICAL RULES:
        1. You MUST use ONLY the EXACT text from the student's explanation
        2. Each "explanation_portion" MUST be a character-for-character substring from the student's response
        3. DO NOT paraphrase, summarize, or create ANY new text
        4. DO NOT add words, remove words, or change punctuation
        5. Copy text VERBATIM - exactly as written by the student

        # How to Segment:
        1. Identify distinct concepts/steps in the student's explanation
        2. Extract the EXACT text for each concept (copy-paste from their response)
        3. Match each exact text portion to its corresponding code lines
        4. One explanation portion can map to multiple code lines
        5. Not all explanation text needs to be used
        6. Not all code needs to be mapped

        # Two Approaches:
        1. Multistructural: If explanation describes steps/implementation details, split into smallest meaningful exact phrases
        2. Relational: If explanation describes overall functionality, use the complete exact description

        # WARNING:
        Every "explanation_portion" must be an EXACT substring of the student's response.
        If you create new text, the segmentation is invalid.

        Here is the code:
        {code}
        """

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
                    {"groups": segmentation_examples["multistructural"]["segmentation"]}
                ),
            }
        )

        segmentation_messages.append(
            {
                "role": "user",
                "content": segmentation_examples["relational"]["description"],
            }
        )
        segmentation_messages.append(
            {
                "role": "assistant",
                "content": json.dumps(
                    {"groups": segmentation_examples["relational"]["segmentation"]}
                ),
            }
        )

        segmentation_messages.append({"role": "user", "content": student_response})

        return segmentation_messages

    def _process_segmentation_response(self, response: Any) -> Dict[str, Any]:
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

    def request_function_generation(self, prompt: str) -> str:
        """Make a request to the Anthropic API for function generation (not yet implemented)."""
        raise NotImplementedError("Anthropic API support not yet implemented")

    def request_segmentation(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Make a request to the Anthropic API for code segmentation (not yet implemented)."""
        raise NotImplementedError("Anthropic API support not yet implemented")


class LlamaRequest(ModelRequest):
    """Model request implementation for Llama API."""

    def __init__(
        self, llama_api_key: str, model: str, temperature: float, num_to_gen: int
    ) -> None:
        super().__init__(None, model, temperature, num_to_gen)
        self.llama_api_key = llama_api_key
        self.base_url = "https://api.llama.com/v1/chat/completions"

    def request_function_generation(self, prompt: str) -> str:
        """Make a request to the Llama API for function generation."""

        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self.llama_api_key}",
        }

        payload = {
            "model": self.model,
            "messages": [
                {
                    "role": "system",
                    "content": "You are a code generation assistant. Generate clean, functional code based on the user's requirements.",
                },
                {"role": "user", "content": prompt},
            ],
            "max_completion_tokens": 1024,
            "temperature": self.temperature,
        }

        try:
            response = requests.post(
                self.base_url, headers=headers, json=payload, timeout=30
            )
            response.raise_for_status()
            result = response.json()

            # Extract the response content
            content = (
                result.get("completion_message", {}).get("content", {}).get("text", "")
            )
            if not content or not isinstance(content, str):
                raise RuntimeError("API returned empty or invalid content")

            # At this point, content is guaranteed to be a str
            result_str: str = content
            return result_str

        except requests.exceptions.RequestException as e:
            raise RuntimeError(f"Error making request to Llama API: {e}") from e
        except (KeyError, TypeError) as e:
            raise RuntimeError(f"Error parsing Llama API response: {e}") from e

    def request_segmentation(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Make a request to the Llama API for code segmentation."""

        # Format the segmentation prompt
        prompt = self._format_segmentation_prompt(
            student_response, code, segmentation_examples
        )

        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self.llama_api_key}",
        }

        payload = {
            "model": self.model,
            "messages": [
                {
                    "role": "system",
                    "content": "You are a code analysis assistant. Analyze code and provide structured explanations in JSON format.",
                },
                {"role": "user", "content": prompt},
            ],
            "max_completion_tokens": 2048,
            "temperature": 0.1,
            "response_format": DEFAULT_SEGMENTATION_RESPONSE_FORMAT,
        }

        try:
            response = requests.post(
                self.base_url, headers=headers, json=payload, timeout=30
            )
            response.raise_for_status()
            result = response.json()

            # Extract the JSON response
            content = (
                result.get("completion_message", {}).get("content", {}).get("text", "")
            )

            # Parse the JSON response
            try:
                parsed_json = json.loads(content)
                if not isinstance(parsed_json, dict):
                    raise ValueError("Expected JSON object, got different type")
                return parsed_json
            except json.JSONDecodeError as e:
                raise ValueError(f"Failed to parse JSON response: {e}") from e

        except requests.exceptions.RequestException as e:
            raise RuntimeError(f"Error making request to Llama API: {e}") from e
        except (KeyError, TypeError) as e:
            raise RuntimeError(f"Error parsing Llama API response: {e}") from e

    def _format_segmentation_prompt(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> str:
        """Format the prompt for code segmentation."""
        prompt = f"""
Map EXACT portions of the student's text to code sections.

CRITICAL: Use ONLY text that appears VERBATIM in the student response.

Student Response (copy text EXACTLY from here):
"{student_response}"

Generated Code:
{code}

ABSOLUTE RULES:
1. Each "explanation_portion" MUST be copied EXACTLY from the student response above
2. DO NOT create, modify, or paraphrase ANY text  
3. Use copy-paste precision - include exact punctuation and spacing
4. Extract text as complete meaningful phrases when possible
5. Every character in explanation_portion must be found in the student response above

SEGMENTATION APPROACH:
- If the student describes multiple steps: extract each step's exact text separately
- If the student gives an overall description: use the whole description as one segment
- Map each extracted text to its corresponding code section
- One explanation portion can map to multiple lines of code
- Not all text needs to be used, not all code needs to be mapped

EXAMPLES showing EXACT text extraction:
{json.dumps(segmentation_examples, indent=2)}

WARNING: If you create any text not found verbatim in the student response, the segmentation is invalid.

Output the segmentation in the required JSON format with "groups" array containing objects with "explanation_portion" and "code" fields.
"""

        return prompt


class OllamaModelRequest(ModelRequest):
    """Model request implementation for Ollama API."""

    def __init__(
        self, ollama_base_url: str, model: str, temperature: float, num_to_gen: int
    ) -> None:
        super().__init__(None, model, temperature, num_to_gen)
        self.ollama_base_url = ollama_base_url

    def request_function_generation(self, prompt: str) -> str:
        """Make a request to the Ollama API for function generation."""
        url = f"{self.ollama_base_url}/api/generate"

        payload = {
            "model": self.model,
            "prompt": prompt,
            "temperature": self.temperature,
            "stream": False,
        }

        try:
            response = requests.post(url, json=payload, timeout=30)
            response.raise_for_status()
            result = response.json()

            # Return the raw generated text
            response_text = result.get("response")
            if response_text is None or not isinstance(response_text, str):
                raise RuntimeError("API returned no response")
            # At this point, response_text is guaranteed to be a str
            result_str: str = response_text
            return result_str

        except requests.exceptions.RequestException as e:
            raise RuntimeError(f"Error making request to Ollama API: {e}") from e

    def request_segmentation(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Make a request to the Ollama API for code segmentation."""
        url = f"{self.ollama_base_url}/api/generate"

        # Format the prompt for segmentation
        prompt = self._format_segmentation_prompt(
            student_response, code, segmentation_examples
        )

        payload = {
            "model": self.model,
            "prompt": prompt,
            "temperature": self.temperature,
            "stream": False,
        }

        try:
            response = requests.post(url, json=payload, timeout=30)
            response.raise_for_status()
            result = response.json()

            # Extract the JSON response
            response_text = result.get("response", "")

            # Try to parse the JSON response
            try:
                # Find JSON content between curly braces
                json_start = response_text.find("{")
                json_end = response_text.rfind("}") + 1
                if 0 <= json_start < json_end:
                    json_str = response_text[json_start:json_end]
                    parsed_json = json.loads(json_str)
                    if not isinstance(parsed_json, dict):
                        raise ValueError("Expected JSON object, got different type")
                    return parsed_json
                raise ValueError("No valid JSON found in response")
            except json.JSONDecodeError as e:
                raise ValueError(f"Failed to parse JSON response: {e}") from e

        except requests.exceptions.RequestException as e:
            raise RuntimeError(f"Error making request to Ollama API: {e}") from e

    def _format_segmentation_prompt(
        self, student_response: str, code: str, segmentation_examples: Dict[str, Any]
    ) -> str:
        """Format the prompt for code segmentation."""
        prompt = f"""Please analyze the following code and provide a structured explanation of its components.

Student Response:
{student_response}

Generated Code:
{code}

Please provide your analysis in the following JSON format:
{{
    "groups": [
        {{
            "explanation_portion": "Explanation of this code section",
            "code": "The relevant code section"
        }}
    ]
}}

Example format from previous analysis:
{json.dumps(segmentation_examples, indent=2)}

Please ensure your response is valid JSON and follows the exact format shown above."""

        return prompt
