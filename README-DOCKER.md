# Docker Guide for eiplgrader

This guide explains how to run eiplgrader in a sandboxed Docker container, optimized for high-scale deployments running thousands of instances.

## Quick Start

### Build the Docker Image

```bash
docker build -f Dockerfile.slim -t eiplgrader:slim .
```

### Run a Simple Example

```bash
docker run --rm \
  -e API_KEY="your-api-key" \
  -e STUDENT_RESPONSE="that adds two numbers and returns the result" \
  -e TEST_CASES='[{"parameters": {"a": 1, "b": 2}, "expected": 3}]' \
  -e LANGUAGE="python" \
  -e FUNCTION_NAME="add_numbers" \
  -e MODEL="gpt-4" \
  -e CLIENT_TYPE="openai" \
  eiplgrader:slim
```

## Using the Helper Script

The `run_grader.sh` script provides a convenient way to run the container:

```bash
# Set environment variables
export API_KEY="your-api-key"
export STUDENT_RESPONSE="that adds two numbers and returns the result"
export TEST_CASES='[{"parameters": {"a": 1, "b": 2}, "expected": 3}]'
export LANGUAGE="python"
export FUNCTION_NAME="add_numbers"
export MODEL="gpt-4"
export CLIENT_TYPE="openai"

# Run with automatic build
./run_grader.sh --build

# Run with custom resource limits
./run_grader.sh --memory 256m --cpus 0.25 --timeout 60
```

## Environment Variables

### Required Variables

- `API_KEY`: API key for the LLM provider
- `STUDENT_RESPONSE`: Natural language description or code explanation
- `TEST_CASES`: JSON array of test cases
- `LANGUAGE`: Programming language (python, javascript, java, c, cpp, go, haskell)
- `FUNCTION_NAME`: Name of the function to generate
- `MODEL`: LLM model to use (e.g., gpt-4, Llama-3.3-70B-Instruct)
- `CLIENT_TYPE`: Client type (openai, meta, anthropic, ollama)

### Optional Variables

- `GEN_TYPE`: Generation type (cgbg or redef), default: cgbg
- `NUM_GENERATIONS`: Number of code variants to generate, default: 1
- `TEMPERATURE`: Model temperature (0.0-1.0), default: 0.0
- `SEGMENTATION`: Enable segmentation (yes/no), default: no
- `SEGMENTATION_FILE`: Path to segmentation examples file
- `INPLACE_MODE`: Test execution mode (0, 1, or 2), default: 0
- `PARAMS`: Function parameters (for redef mode)
- `ASSUMPTIONS`: Function assumptions (for redef mode)

## Test Case Format

### Python/JavaScript (Type Inference Supported)
```json
[
  {
    "parameters": {"a": 1, "b": 2},
    "expected": 3
  },
  {
    "parameters": {"numbers": [1, 2, 3, 4, 5]},
    "expected": 15
  }
]
```

### Static Languages (Types Required)
```json
[
  {
    "parameters": {"a": 1, "b": 2},
    "parameter_types": {"a": "int", "b": "int"},
    "expected": 3,
    "expected_type": "int"
  }
]
```

## Django Integration Example

### Basic Synchronous View

```python
import json
import subprocess
import uuid
from django.http import JsonResponse
from django.conf import settings

def grade_submission(request):
    data = json.loads(request.body)
    
    # Run Docker container with security limits
    container_name = f"eiplgrader-{uuid.uuid4()}"
    docker_cmd = [
        "docker", "run",
        "--rm",
        "--name", container_name,
        "--memory", "512m",
        "--cpus", "0.5",
        "--network", "none",
        "--read-only",
        "--tmpfs", "/tmp:rw,noexec,nosuid,size=100m",
        "-e", f"API_KEY={settings.EIPLGRADER_API_KEY}",
        "-e", f"STUDENT_RESPONSE={data['student_response']}",
        "-e", f"TEST_CASES={json.dumps(data['test_cases'])}",
        "-e", f"LANGUAGE={data['language']}",
        "-e", f"FUNCTION_NAME={data['function_name']}",
        "-e", f"MODEL={data['model']}",
        "-e", f"CLIENT_TYPE={data['client_type']}",
        "eiplgrader:slim"
    ]
    
    try:
        result = subprocess.run(
            docker_cmd,
            capture_output=True,
            text=True,
            timeout=30
        )
        grading_result = json.loads(result.stdout)
        return JsonResponse(grading_result)
    except subprocess.TimeoutExpired:
        return JsonResponse({"error": "Grading timeout"}, status=408)
    except Exception as e:
        return JsonResponse({"error": str(e)}, status=500)
```

### Asynchronous with Celery

```python
from celery import shared_task
import subprocess
import json

@shared_task(bind=True, max_retries=3)
def grade_submission_async(self, submission_data):
    try:
        # Similar Docker execution as above
        docker_cmd = [
            "docker", "run",
            "--rm",
            "--memory", "512m",
            "--cpus", "0.5",
            "--network", "none",
            "--read-only",
            "-e", f"API_KEY={submission_data['api_key']}",
            # ... other environment variables ...
            "eiplgrader:slim"
        ]
        
        result = subprocess.run(
            docker_cmd,
            capture_output=True,
            text=True,
            timeout=30
        )
        
        return json.loads(result.stdout)
        
    except Exception as exc:
        raise self.retry(exc=exc, countdown=60)
```

## Docker Compose Usage

For local development and testing:

```bash
# Create .env file
cat > .env << EOF
API_KEY=your-api-key
STUDENT_RESPONSE="that calculates factorial"
TEST_CASES='[{"parameters": {"n": 5}, "expected": 120}]'
LANGUAGE=python
FUNCTION_NAME=factorial
MODEL=gpt-4
CLIENT_TYPE=openai
EOF

# Run with docker-compose
docker-compose up grader
```

## Security Features

The Docker container includes multiple security layers:

1. **Resource Limits**: Memory and CPU constraints prevent resource exhaustion
2. **Network Isolation**: `--network none` prevents external network access
3. **Read-only Filesystem**: Prevents persistent modifications
4. **Non-root User**: Runs as unprivileged user `grader`
5. **Temporary Storage**: Limited tmpfs for temporary files
6. **No New Privileges**: Prevents privilege escalation

## Performance Optimization

### Image Size
- Base image: ~50MB (python:3.13-alpine)
- With all languages: ~300-400MB
- Startup time: <2 seconds

### Scaling Recommendations
1. **Pre-pull Images**: Ensure all nodes have the image cached
2. **Resource Limits**: Set appropriate limits based on workload
3. **Container Pools**: Consider using container orchestration (Kubernetes, Swarm)
4. **Parallel Execution**: Run multiple containers concurrently

### Example Kubernetes Deployment

```yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: eiplgrader-job
spec:
  parallelism: 100  # Run 100 concurrent grading jobs
  template:
    spec:
      containers:
      - name: grader
        image: eiplgrader:slim
        resources:
          limits:
            memory: "512Mi"
            cpu: "500m"
        env:
        - name: API_KEY
          valueFrom:
            secretKeyRef:
              name: eiplgrader-secrets
              key: api-key
        # ... other environment variables
      restartPolicy: Never
```

## Output Format

The container outputs JSON to stdout:

```json
{
  "success": true,
  "error": null,
  "generated_code": [
    "def add_numbers(a, b):\n    return a + b"
  ],
  "test_results": [
    {
      "variant": 1,
      "code": "def add_numbers(a, b):\n    return a + b",
      "total_tests": 4,
      "passed": 4,
      "failed": 0,
      "errors": 0,
      "test_details": [...]
    }
  ],
  "language": "python"
}
```

## Troubleshooting

### Common Issues

1. **Container Timeout**: Increase timeout with `--timeout` flag
2. **Memory Errors**: Increase memory limit with `--memory` flag
3. **Permission Denied**: Ensure Docker daemon is accessible
4. **Invalid JSON**: Validate TEST_CASES JSON format

### Debug Mode

Run with additional output:

```bash
docker run --rm \
  -e PYTHONUNBUFFERED=1 \
  -e API_KEY="..." \
  # ... other variables ...
  eiplgrader:slim
```

## Integration Testing

Test the complete flow:

```bash
# Python example
export API_KEY="your-api-key"
export STUDENT_RESPONSE="that reverses a string"
export TEST_CASES='[
  {"parameters": {"s": "hello"}, "expected": "olleh"},
  {"parameters": {"s": "world"}, "expected": "dlrow"}
]'
export LANGUAGE="python"
export FUNCTION_NAME="reverse_string"
export MODEL="gpt-4"
export CLIENT_TYPE="openai"

./run_grader.sh --build
```

## Advanced Usage

### Multiple Language Tests

Create a script to test multiple languages:

```bash
#!/bin/bash
languages=("python" "javascript" "java" "go")
for lang in "${languages[@]}"; do
  echo "Testing $lang..."
  LANGUAGE=$lang ./run_grader.sh
done
```

### Batch Processing

Process multiple submissions:

```python
import concurrent.futures
import subprocess

def grade_single(submission):
    # Docker command for single submission
    pass

with concurrent.futures.ThreadPoolExecutor(max_workers=50) as executor:
    results = list(executor.map(grade_single, submissions))
```

## Additional Resources

- [Django Integration Plan](docs/DOCKER_DJANGO_INTEGRATION_PLAN.md)
- [Main Documentation](README.md)
- [API Documentation](docs/developer/api/index.md)