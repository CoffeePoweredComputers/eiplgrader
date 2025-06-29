---
layout: default
title: Docker Deployment
parent: User Guide
nav_order: 6
---

# Docker Deployment Guide
{: .no_toc }

Deploy EiplGrader in secure, isolated containers for production-grade autograding at scale.
{: .fs-6 .fw-300 }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Overview

The EiplGrader Docker image provides a secure, isolated environment for running autograding tasks. It's optimized for high-scale deployments where you need to run thousands of grading instances safely and efficiently.

### Key Features

- **Complete Language Support**: All supported languages pre-installed (Python, JavaScript, Java, C/C++, Go, Haskell)
- **Security Hardened**: Runs as non-root user with read-only filesystem
- **Resource Efficient**: Alpine Linux base, ~300MB total size
- **Fast Startup**: < 2 seconds to start and execute
- **Network Isolated**: No external network access by default
- **Resource Limited**: Built-in memory and CPU constraints

## Quick Start

### Build the Image

```bash
git clone https://github.com/hamiltonfour/eiplgrader.git
cd eiplgrader
docker build -t eiplgrader:latest .
```

### Basic Usage

```bash
docker run --rm \
  -e API_KEY="your-api-key" \
  -e STUDENT_RESPONSE="that adds two numbers and returns the result" \
  -e TEST_CASES='[{"parameters": {"a": 1, "b": 2}, "expected": 3}]' \
  -e LANGUAGE="python" \
  -e FUNCTION_NAME="add_numbers" \
  -e MODEL="gpt-4" \
  -e CLIENT_TYPE="openai" \
  eiplgrader:latest
```

### Using the Helper Script

The `run_grader.sh` script provides a convenient interface with built-in safety features:

```bash
# Set required environment variables
export API_KEY="your-api-key"
export STUDENT_RESPONSE="that calculates factorial"
export TEST_CASES='[{"parameters": {"n": 5}, "expected": 120}]'
export LANGUAGE="python"
export FUNCTION_NAME="factorial"
export MODEL="gpt-4"
export CLIENT_TYPE="openai"

# Run with automatic build
./run_grader.sh --build

# Run with custom resource limits
./run_grader.sh --memory 256m --cpus 0.25 --timeout 60
```

## Environment Variables

### Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `API_KEY` | API key for LLM provider | `sk-...` |
| `STUDENT_RESPONSE` | Natural language description | `"that sorts a list"` |
| `TEST_CASES` | JSON array of test cases | `[{"parameters": {...}, "expected": ...}]` |
| `LANGUAGE` | Programming language | `python`, `javascript`, `java`, etc. |
| `FUNCTION_NAME` | Function to generate | `sort_list` |
| `MODEL` | LLM model name | `gpt-4`, `Llama-3.3-70B` |
| `CLIENT_TYPE` | Provider type | `openai`, `meta`, `anthropic` |

### Optional Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GEN_TYPE` | Generation type | `cgbg` |
| `NUM_GENERATIONS` | Number of variants | `1` |
| `TEMPERATURE` | Model temperature | `0.0` |
| `SEGMENTATION` | Enable segmentation | `no` |
| `INPLACE_MODE` | Test execution mode | `0` |

## Security Features

The Docker container implements multiple security layers:

### 1. Non-Root User
```dockerfile
# Runs as user 'grader' (UID 1000)
USER grader
```

### 2. Read-Only Filesystem
```bash
docker run --read-only ...
```

### 3. Network Isolation
```bash
docker run --network none ...
```

### 4. Resource Limits
```bash
docker run --memory 512m --cpus 0.5 ...
```

### 5. Temporary Storage
```bash
docker run --tmpfs /tmp:rw,noexec,nosuid,size=100m ...
```

## Test Case Formats

### Dynamic Languages (Python/JavaScript)

Type inference is automatic:

```json
[
  {
    "parameters": {"numbers": [1, 2, 3]},
    "expected": 6
  }
]
```

### Static Languages (Java/C++/Go/Haskell)

Explicit types required:

```json
[
  {
    "parameters": {"n": 5},
    "parameter_types": {"n": "int"},
    "expected": 120,
    "expected_type": "int"
  }
]
```

## Integration Examples

### Django Integration

Basic synchronous view:

```python
import json
import subprocess
from django.http import JsonResponse

def grade_submission(request):
    data = json.loads(request.body)
    
    docker_cmd = [
        "docker", "run", "--rm",
        "--memory", "512m",
        "--cpus", "0.5",
        "--network", "none",
        "--read-only",
        "-e", f"API_KEY={settings.EIPLGRADER_API_KEY}",
        "-e", f"STUDENT_RESPONSE={data['response']}",
        "-e", f"TEST_CASES={json.dumps(data['tests'])}",
        "-e", f"LANGUAGE={data['language']}",
        "-e", f"FUNCTION_NAME={data['function']}",
        "-e", f"MODEL={data['model']}",
        "-e", f"CLIENT_TYPE={data['client']}",
        "eiplgrader:latest"
    ]
    
    try:
        result = subprocess.run(
            docker_cmd,
            capture_output=True,
            text=True,
            timeout=30
        )
        return JsonResponse(json.loads(result.stdout))
    except subprocess.TimeoutExpired:
        return JsonResponse({"error": "Timeout"}, status=408)
```

### Celery Async Task

```python
from celery import shared_task
import subprocess
import json

@shared_task(bind=True, max_retries=3)
def grade_submission_async(self, submission_data):
    docker_cmd = [
        "docker", "run", "--rm",
        "--memory", "512m",
        "--cpus", "0.5",
        "--network", "none",
        "--read-only",
        "-e", f"API_KEY={submission_data['api_key']}",
        # ... other environment variables ...
        "eiplgrader:latest"
    ]
    
    result = subprocess.run(
        docker_cmd,
        capture_output=True,
        text=True,
        timeout=30
    )
    
    return json.loads(result.stdout)
```

### Kubernetes Job

```yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: grade-submissions
spec:
  parallelism: 50
  template:
    spec:
      containers:
      - name: grader
        image: eiplgrader:latest
        resources:
          limits:
            memory: "512Mi"
            cpu: "500m"
          requests:
            memory: "256Mi"
            cpu: "250m"
        env:
        - name: API_KEY
          valueFrom:
            secretKeyRef:
              name: grader-secrets
              key: api-key
        # ... other env vars
      restartPolicy: Never
```

## Docker Compose

For local development and testing:

```yaml
# docker-compose.yml
version: '3.8'

services:
  grader:
    build: .
    image: eiplgrader:latest
    environment:
      - API_KEY
      - STUDENT_RESPONSE
      - TEST_CASES
      - LANGUAGE
      - FUNCTION_NAME
      - MODEL
      - CLIENT_TYPE
    deploy:
      resources:
        limits:
          cpus: '0.5'
          memory: 512M
    security_opt:
      - no-new-privileges:true
    read_only: true
    tmpfs:
      - /tmp:rw,noexec,nosuid,size=100m
    network_mode: none
```

Usage:
```bash
# Create .env file
cat > .env << EOF
API_KEY=your-api-key
STUDENT_RESPONSE="that reverses a string"
TEST_CASES='[{"parameters": {"s": "hello"}, "expected": "olleh"}]'
LANGUAGE=python
FUNCTION_NAME=reverse_string
MODEL=gpt-4
CLIENT_TYPE=openai
EOF

# Run
docker-compose up grader
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
      "total_tests": 2,
      "passed": 2,
      "failed": 0,
      "errors": 0,
      "test_details": [
        {
          "test_index": 0,
          "passed": true,
          "expected": 3,
          "actual": 3
        }
      ]
    }
  ],
  "language": "python"
}
```

## Performance Optimization

### Image Optimization

- **Multi-stage build**: Separate build and runtime stages
- **Alpine Linux**: Minimal base image
- **Wheel caching**: Pre-compiled Python packages
- **Layer optimization**: Minimal layer count

### Scaling Recommendations

1. **Pre-pull images** on all nodes:
   ```bash
   docker pull eiplgrader:latest
   ```

2. **Use container orchestration** for large scale:
   - Kubernetes Jobs for batch processing
   - Docker Swarm for simpler deployments

3. **Implement connection pooling** for API calls

4. **Monitor resource usage**:
   ```bash
   docker stats --no-stream
   ```

## Troubleshooting

### Common Issues

**Container Timeout**
```bash
# Increase timeout
./run_grader.sh --timeout 60
```

**Memory Errors**
```bash
# Increase memory limit
./run_grader.sh --memory 1g
```

**Invalid JSON**
```bash
# Validate your TEST_CASES JSON
echo $TEST_CASES | jq .
```

### Debug Mode

Enable verbose output:
```bash
docker run --rm \
  -e PYTHONUNBUFFERED=1 \
  -e DEBUG=1 \
  # ... other variables
  eiplgrader:latest
```

### Container Logs

```bash
# View logs for a running container
docker logs container-name

# Follow logs
docker logs -f container-name
```

## Advanced Usage

### Batch Processing

Process multiple submissions efficiently:

```python
import concurrent.futures
import subprocess

def grade_single(submission):
    docker_cmd = [
        "docker", "run", "--rm",
        # ... docker arguments
    ]
    result = subprocess.run(docker_cmd, capture_output=True)
    return json.loads(result.stdout)

# Process 100 submissions in parallel
with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
    results = list(executor.map(grade_single, submissions))
```

### Custom Dockerfile

Extend the base image:

```dockerfile
FROM eiplgrader:latest

# Add custom dependencies
USER root
RUN apk add --no-cache your-package

# Switch back to grader user
USER grader

# Add custom configuration
COPY custom-config.json /app/
```

### Volume Mounts

For development testing:

```bash
docker run --rm \
  -v $(pwd)/test_files:/app/tests:ro \
  -e TEST_FILE="/app/tests/test1.json" \
  # ... other options
  eiplgrader:latest
```

## Best Practices

1. **Always set resource limits** to prevent runaway containers
2. **Use --rm flag** to automatically clean up containers
3. **Implement proper error handling** for container failures
4. **Monitor container metrics** in production
5. **Use secrets management** for API keys
6. **Implement retry logic** for transient failures
7. **Log container output** for debugging

## Additional Resources

- [Main Documentation](../)
- [Language Support Details](languages.md)
- [Error Handling Guide](errors.md)
- [GitHub Repository](https://github.com/hamiltonfour/eiplgrader)