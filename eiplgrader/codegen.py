import openai

SYSTEM_PROMPT = """
Pretend you are an introductory CS student learning python for the
very first time. Your have a rudementary understanding of functions,
loops, variables, and conditionals.  Create a function, called foo,
according to the following prompt:

Create a function foo that {student_prompt}

Additional information on the code the students prompt refers to:
{assumptions}

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
                 temperature=0):

        self.model = model
        self.temperature = temperature
        self.client = openai.OpenAI(api_key=api_key)

    def generate_code(self, prompt):

        prompt = [
            {
                "role": "user", 
                "content": f"{SYSTEM_PROMPT.format(student_prompt=prompt, assumptions='None')}"
            }
        ]

        response = self.client.chat.completions.create(
              model=self.model,
              messages=prompt,
              temperature=self.temperature,
        )

        generated_code = response.choices[0].message.content.replace("```python", "").replace("```", "").strip()
        return generated_code

