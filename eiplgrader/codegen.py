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
                 model="gpt-4o", 
                 temperature=0,
                 system_prompt=SYSTEM_PROMPT,
                 num_to_generate=1):

        self.model = model
        self.temperature = temperature
        self.client = openai.OpenAI(api_key=api_key)
        self.system_prompt = system_prompt
        self.num_to_generate = num_to_generate

    def generate_code(self, student_response):

        prompt = [
            {
                "role": "user", 
                "content": self.system_prompt.format(student_response)
            }
        ]

        response = self.client.chat.completions.create(
              model=self.model,
              messages=prompt,
              temperature=self.temperature,
        )

        if self.num_to_generate > 1:
            generated_code = [response.choices[i].message.content.replace("```python", "").replace("```", "").strip() for i in range(self.num_to_generate)]
        else:
            generated_code = response.choices[0].message.content.replace("```python", "").replace("```", "").strip()
        return generated_code

