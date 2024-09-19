FROM python:3.12

ENV PYTHONUNBUFFERED=1
RUN pip install eiplgrader==0.1.7
COPY run_tests.py /app/run_tests.py
WORKDIR /app

CMD ["python", "run_tests.py"]
