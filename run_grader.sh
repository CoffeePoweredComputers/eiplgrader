#!/bin/bash
# Helper script to run eiplgrader in Docker container
# Usage: ./run_grader.sh

set -e

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo "Error: Docker is not installed"
    exit 1
fi

# Function to display usage
usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Run eiplgrader in a sandboxed Docker container.

Required environment variables:
  API_KEY              API key for the LLM provider
  STUDENT_RESPONSE     Natural language description or code explanation
  TEST_CASES           JSON array of test cases
  LANGUAGE             Programming language (python, javascript, java, c, cpp, go, haskell)
  FUNCTION_NAME        Name of the function to generate
  MODEL                LLM model to use
  CLIENT_TYPE          Client type (openai, meta, anthropic, ollama)

Optional environment variables:
  GEN_TYPE             Generation type (cgbg or redef), default: cgbg
  NUM_GENERATIONS      Number of code variants to generate, default: 1
  TEMPERATURE          Model temperature (0.0-1.0), default: 0.0
  SEGMENTATION         Enable segmentation (yes/no), default: no
  SEGMENTATION_FILE    Path to segmentation examples file
  INPLACE_MODE         Test execution mode (0, 1, or 2), default: 0
  PARAMS               Function parameters (for redef mode)
  ASSUMPTIONS          Function assumptions (for redef mode)

Options:
  -h, --help           Show this help message
  -b, --build          Build the Docker image before running
  -n, --name NAME      Container name (default: auto-generated)
  -m, --memory LIMIT   Memory limit (default: 512m)
  -c, --cpus LIMIT     CPU limit (default: 0.5)
  -t, --timeout SEC    Timeout in seconds (default: 30)

Example:
  export API_KEY="your-api-key"
  export STUDENT_RESPONSE="that adds two numbers and returns the result"
  export TEST_CASES='[{"parameters": {"a": 1, "b": 2}, "expected": 3}]'
  export LANGUAGE="python"
  export FUNCTION_NAME="add_numbers"
  export MODEL="gpt-4"
  export CLIENT_TYPE="openai"
  
  $0 --build
EOF
}

# Default values
BUILD=false
CONTAINER_NAME=""
MEMORY_LIMIT="512m"
CPU_LIMIT="0.5"
TIMEOUT=30

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -b|--build)
            BUILD=true
            shift
            ;;
        -n|--name)
            CONTAINER_NAME="$2"
            shift 2
            ;;
        -m|--memory)
            MEMORY_LIMIT="$2"
            shift 2
            ;;
        -c|--cpus)
            CPU_LIMIT="$2"
            shift 2
            ;;
        -t|--timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Validate required environment variables
REQUIRED_VARS=(
    "API_KEY"
    "STUDENT_RESPONSE"
    "TEST_CASES"
    "LANGUAGE"
    "FUNCTION_NAME"
    "MODEL"
    "CLIENT_TYPE"
)

for var in "${REQUIRED_VARS[@]}"; do
    if [ -z "${!var}" ]; then
        echo "Error: Required environment variable $var is not set"
        exit 1
    fi
done

# Build image if requested
if [ "$BUILD" = true ]; then
    echo "Building Docker image..."
    docker build -t eiplgrader:latest .
fi

# Generate container name if not provided
if [ -z "$CONTAINER_NAME" ]; then
    CONTAINER_NAME="eiplgrader-$(date +%s)-$$"
fi

# Prepare Docker command
DOCKER_CMD=(
    "docker" "run"
    "--rm"
    "--name" "$CONTAINER_NAME"
    "--memory" "$MEMORY_LIMIT"
    "--cpus" "$CPU_LIMIT"
    "--network" "none"
    "--read-only"
    "--tmpfs" "/tmp:rw,noexec,nosuid,size=100m"
    "-e" "API_KEY"
    "-e" "STUDENT_RESPONSE"
    "-e" "TEST_CASES"
    "-e" "LANGUAGE"
    "-e" "FUNCTION_NAME"
    "-e" "MODEL"
    "-e" "CLIENT_TYPE"
)

# Add optional environment variables if set
[ -n "$GEN_TYPE" ] && DOCKER_CMD+=("-e" "GEN_TYPE")
[ -n "$NUM_GENERATIONS" ] && DOCKER_CMD+=("-e" "NUM_GENERATIONS")
[ -n "$TEMPERATURE" ] && DOCKER_CMD+=("-e" "TEMPERATURE")
[ -n "$SEGMENTATION" ] && DOCKER_CMD+=("-e" "SEGMENTATION")
[ -n "$SEGMENTATION_FILE" ] && DOCKER_CMD+=("-e" "SEGMENTATION_FILE")
[ -n "$INPLACE_MODE" ] && DOCKER_CMD+=("-e" "INPLACE_MODE")
[ -n "$PARAMS" ] && DOCKER_CMD+=("-e" "PARAMS")
[ -n "$ASSUMPTIONS" ] && DOCKER_CMD+=("-e" "ASSUMPTIONS")

# Add image name
DOCKER_CMD+=("eiplgrader:latest")

# Run container with timeout
echo "Running eiplgrader in sandboxed container..."
if timeout "$TIMEOUT" "${DOCKER_CMD[@]}"; then
    exit 0
else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 124 ]; then
        echo "Error: Container execution timed out after ${TIMEOUT} seconds"
    else
        echo "Error: Container exited with code $EXIT_CODE"
    fi
    exit $EXIT_CODE
fi