---
layout: default
title: Docker Usage
parent: User Guide
nav_order: 5
---

# Docker Usage

Run EiplGrader securely in containerized environments for production grading scenarios.

## Why Use Docker?

EiplGrader generates and executes arbitrary code from natural language descriptions. Running it in a Docker container provides:

- **Security Isolation**: Code runs in a sandboxed environment
- **Resource Limits**: Control CPU, memory, and execution time
- **Consistency**: Same environment across different systems
- **Easy Deployment**: Simple distribution and scaling

## Quick Start

### Pull the Official Image

```bash
docker pull coffeepwrdcomputers/eiplgrader
```

### Basic Usage

```bash
# Run with API key and prompt
docker run -e API_KEY="your-openai-api-key" \
           -e PROMPT="adds two numbers" \
           -e TEST_CASES_FILE="tests.json" \
           -v $(pwd)/tests.json:/app/tests.json \
           coffeepwrdcomputers/eiplgrader
```

### Using Pre-generated Code

```bash
# Test existing code without generation
docker run -e USER_CODE="def foo(a, b): return a + b" \
           -e TEST_CASES_FILE="tests.json" \
           -v $(pwd)/tests.json:/app/tests.json \
           coffeepwrdcomputers/eiplgrader
```

## Test Cases File Format

Create a `tests.json` file with your test cases:

```json
{
  "test_cases": [
    {
      "parameters": {"a": 1, "b": 2},
      "expected": 3
    },
    {
      "parameters": {"a": -1, "b": 1},
      "expected": 0
    },
    {
      "parameters": {"a": 0, "b": 0},
      "expected": 0
    }
  ],
  "function_name": "add_numbers",
  "language": "python"
}
```

## Environment Variables

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `API_KEY` | OpenAI API key | Yes (if generating) | - |
| `PROMPT` | Natural language description | Yes (if generating) | - |
| `USER_CODE` | Pre-generated code to test | Yes (if not generating) | - |
| `TEST_CASES_FILE` | Path to test cases JSON | Yes | - |
| `LANGUAGE` | Programming language | No | `python` |
| `FUNCTION_NAME` | Function name to test | No | `foo` |
| `GEN_TYPE` | Generation type (cgbg/redef) | No | `cgbg` |
| `MODEL` | LLM model to use | No | `gpt-4o` |
| `NUM_TO_GEN` | Number of variants | No | `1` |

## Advanced Docker Usage

### Custom Dockerfile

Create a custom Dockerfile for specific requirements:

```dockerfile
FROM coffeepwrdcomputers/eiplgrader

# Install additional languages or tools
RUN apt-get update && apt-get install -y \
    golang-go \
    openjdk-11-jdk \
    ghc

# Copy custom configuration
COPY config.json /app/config.json

# Set default environment
ENV LANGUAGE=java
ENV MODEL=gpt-4o

ENTRYPOINT ["python", "runner.py"]
```

### Resource Limits

Limit container resources for safety:

```bash
docker run --memory="512m" \
           --cpus="1.0" \
           --timeout=60 \
           -e API_KEY="your-key" \
           -e PROMPT="complex algorithm" \
           -e TEST_CASES_FILE="tests.json" \
           -v $(pwd)/tests.json:/app/tests.json \
           coffeepwrdcomputers/eiplgrader
```

### Volume Mounts

Mount directories for persistent data:

```bash
# Mount multiple test case files
docker run -v $(pwd)/test_cases:/app/test_cases \
           -v $(pwd)/results:/app/results \
           -e API_KEY="your-key" \
           -e PROMPT="sorting algorithm" \
           -e TEST_CASES_FILE="/app/test_cases/sort_tests.json" \
           coffeepwrdcomputers/eiplgrader > /app/results/output.json
```

## Docker Compose

For complex setups, use Docker Compose:

```yaml
# docker-compose.yml
version: '3.8'

services:
  eiplgrader:
    image: coffeepwrdcomputers/eiplgrader
    environment:
      - API_KEY=${OPENAI_API_KEY}
      - LANGUAGE=python
      - MODEL=gpt-4o
    volumes:
      - ./test_cases:/app/test_cases
      - ./results:/app/results
    deploy:
      resources:
        limits:
          cpus: '1.0'
          memory: 512M
        reservations:
          cpus: '0.5'
          memory: 256M

  # Additional services (database, queue, etc.)
  redis:
    image: redis:alpine
    ports:
      - "6379:6379"
```

Run with:
```bash
docker-compose up
```

## Batch Processing

Process multiple student submissions:

```bash
#!/bin/bash
# batch_grade.sh

# Directory structure:
# submissions/
#   student1.txt
#   student2.txt
#   ...
# test_cases.json

for student_file in submissions/*.txt; do
    student_name=$(basename "$student_file" .txt)
    prompt=$(cat "$student_file")
    
    echo "Processing $student_name..."
    
    docker run -e API_KEY="$OPENAI_API_KEY" \
               -e PROMPT="$prompt" \
               -e TEST_CASES_FILE="tests.json" \
               -v $(pwd)/test_cases.json:/app/tests.json \
               coffeepwrdcomputers/eiplgrader \
               > "results/${student_name}_result.json"
done
```

## Multi-Language Support

Run tests in different languages:

```bash
# Python
docker run -e LANGUAGE="python" \
           -e USER_CODE="def foo(x): return x * 2" \
           -e TEST_CASES_FILE="python_tests.json" \
           -v $(pwd)/python_tests.json:/app/python_tests.json \
           coffeepwrdcomputers/eiplgrader

# Java
docker run -e LANGUAGE="java" \
           -e USER_CODE="public static int foo(int x) { return x * 2; }" \
           -e TEST_CASES_FILE="java_tests.json" \
           -v $(pwd)/java_tests.json:/app/java_tests.json \
           coffeepwrdcomputers/eiplgrader

# JavaScript
docker run -e LANGUAGE="javascript" \
           -e USER_CODE="function foo(x) { return x * 2; }" \
           -e TEST_CASES_FILE="js_tests.json" \
           -v $(pwd)/js_tests.json:/app/js_tests.json \
           coffeepwrdcomputers/eiplgrader
```

## Security Best Practices

### 1. Network Isolation

```bash
# Create isolated network
docker network create --driver bridge grading_network

# Run container with no internet access
docker run --network=grading_network \
           --network-alias=grader \
           -e API_KEY="your-key" \
           coffeepwrdcomputers/eiplgrader
```

### 2. Read-Only Filesystem

```bash
# Make container filesystem read-only
docker run --read-only \
           --tmpfs /tmp \
           --tmpfs /app/temp \
           -e API_KEY="your-key" \
           coffeepwrdcomputers/eiplgrader
```

### 3. User Permissions

```bash
# Run as non-root user
docker run --user 1000:1000 \
           -e API_KEY="your-key" \
           coffeepwrdcomputers/eiplgrader
```

### 4. Seccomp Profiles

```bash
# Use security profiles
docker run --security-opt seccomp=eiplgrader-seccomp.json \
           -e API_KEY="your-key" \
           coffeepwrdcomputers/eiplgrader
```

## Monitoring and Logging

### Container Logs

```bash
# View logs
docker logs container_id

# Follow logs
docker logs -f container_id

# Save logs
docker logs container_id > grading.log
```

### Health Checks

Add health checks to your Dockerfile:

```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
  CMD python -c "import sys; sys.exit(0)" || exit 1
```

### Metrics Collection

```yaml
# docker-compose with monitoring
version: '3.8'

services:
  eiplgrader:
    image: coffeepwrdcomputers/eiplgrader
    labels:
      - "prometheus.io/scrape=true"
      - "prometheus.io/port=8080"
    
  prometheus:
    image: prom/prometheus
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml
    ports:
      - "9090:9090"
```

## Kubernetes Deployment

Deploy EiplGrader on Kubernetes:

```yaml
# eiplgrader-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: eiplgrader
spec:
  replicas: 3
  selector:
    matchLabels:
      app: eiplgrader
  template:
    metadata:
      labels:
        app: eiplgrader
    spec:
      containers:
      - name: eiplgrader
        image: coffeepwrdcomputers/eiplgrader
        resources:
          limits:
            memory: "512Mi"
            cpu: "1000m"
          requests:
            memory: "256Mi"
            cpu: "500m"
        env:
        - name: API_KEY
          valueFrom:
            secretKeyRef:
              name: eiplgrader-secrets
              key: openai-api-key
        volumeMounts:
        - name: test-cases
          mountPath: /app/test_cases
      volumes:
      - name: test-cases
        configMap:
          name: test-cases-config
```

## Troubleshooting

### Common Issues

1. **Container exits immediately**
   ```bash
   # Check logs
   docker logs container_id
   
   # Run interactively for debugging
   docker run -it --entrypoint /bin/bash coffeepwrdcomputers/eiplgrader
   ```

2. **Permission denied errors**
   ```bash
   # Fix volume permissions
   docker run -v $(pwd)/tests.json:/app/tests.json:ro \
              coffeepwrdcomputers/eiplgrader
   ```

3. **Out of memory**
   ```bash
   # Increase memory limit
   docker run --memory="1g" coffeepwrdcomputers/eiplgrader
   ```

4. **Timeout issues**
   ```bash
   # Increase timeout in test cases
   {
     "test_cases": [{
       "parameters": {"n": 1000000},
       "expected": "result",
       "timeout": 60
     }]
   }
   ```

### Debug Mode

Run container in debug mode:

```bash
# Set debug environment variable
docker run -e DEBUG=true \
           -e API_KEY="your-key" \
           -e PROMPT="test function" \
           coffeepwrdcomputers/eiplgrader
```

## Performance Optimization

### 1. Layer Caching

Build custom images efficiently:

```dockerfile
# Base layer (changes rarely)
FROM coffeepwrdcomputers/eiplgrader AS base

# Dependencies layer
RUN pip install --no-cache-dir additional-packages

# Application layer (changes frequently)
COPY custom_code.py /app/
```

### 2. Multi-Stage Builds

```dockerfile
# Build stage
FROM python:3.9 AS builder
WORKDIR /build
COPY requirements.txt .
RUN pip wheel --no-cache-dir --no-deps --wheel-dir /build/wheels -r requirements.txt

# Runtime stage
FROM python:3.9-slim
COPY --from=builder /build/wheels /wheels
RUN pip install --no-cache /wheels/*
```

### 3. Container Reuse

For high-volume grading:

```python
# grading_service.py
import docker
import json

client = docker.from_env()

# Create long-running container
container = client.containers.run(
    "coffeepwrdcomputers/eiplgrader",
    detach=True,
    stdin_open=True,
    command="python interactive_grader.py"
)

# Send multiple grading requests
for submission in submissions:
    container.exec_run(f"grade '{submission}'")
```

## Integration Examples

### GitHub Actions

```yaml
# .github/workflows/grade.yml
name: Auto-grade submissions
on:
  pull_request:
    paths:
      - 'submissions/**.py'

jobs:
  grade:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Grade submission
        run: |
          docker run -e API_KEY=${{ secrets.OPENAI_API_KEY }} \
                     -e USER_CODE="$(cat ${{ github.event.pull_request.changed_files[0] }})" \
                     -e TEST_CASES_FILE="tests.json" \
                     -v $(pwd)/tests.json:/app/tests.json \
                     coffeepwrdcomputers/eiplgrader
```

### CI/CD Pipeline

```bash
# Jenkinsfile
pipeline {
    agent {
        docker {
            image 'coffeepwrdcomputers/eiplgrader'
            args '-v $WORKSPACE:/app/workspace'
        }
    }
    stages {
        stage('Grade') {
            steps {
                sh 'python grade_all.py'
            }
        }
    }
}
```

## Next Steps

- Review [Security Best Practices](#security-best-practices) before production use
- Explore [Kubernetes Deployment](#kubernetes-deployment) for scaling
- See [Developer Documentation](../developer/) for custom Docker builds
- Learn about [Error Handling](errors.md) for container debugging