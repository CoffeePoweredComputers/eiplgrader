import openai

SYSTEM_PROMPT = """
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

class CodeGenerator:

    def __init__(self, 
                 api_key,
                 model="gpt-4o-mini", 
                 temperature=0,
                 system_prompt=SYSTEM_PROMPT
                 ):

        self.model = model
        self.temperature = temperature
        self.client = openai.OpenAI(api_key=api_key)
        self.system_prompt = system_prompt

    def generate_code(self, student_response):

        prompt = [
            {
                "role": "user", 
                "content": f"{self.system_prompt.format(student_response)}"
            }
        ]

        response = self.client.chat.completions.create(
              model=self.model,
              messages=prompt,
              temperature=self.temperature,
        )

        generated_code = response.choices[0].message.content.replace("```python", "").replace("```", "").strip()
        return generated_code

