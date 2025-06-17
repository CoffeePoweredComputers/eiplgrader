#!/bin/bash
# Lint script for eiplgrader
# Runs all code quality checks from CI workflow locally

set -e  # Exit on error

# Change to the script's directory
cd "$(dirname "${BASH_SOURCE[0]}")"

# Check for virtual environment
if [ -d "./venv" ]; then
    echo "Using existing virtual environment..."
    source ./venv/bin/activate
else
    echo "Creating virtual environment..."
    python -m venv venv
    source ./venv/bin/activate
    pip install --upgrade pip
    pip install pylint black mypy pytest pytest-cov
    # Install project dependencies
    if [ -f "requirements.txt" ]; then
        pip install -r requirements.txt
    fi
    # If poetry is available, use it to install dependencies
    if command -v poetry &> /dev/null; then
        poetry install --with dev
    fi
    # Explicitly install openai to ensure it's available
    pip install openai
fi

echo "========== Running Linting Checks =========="

echo ""
echo "Running pylint..."
pylint --disable=C0111,C0103,E0401,R0913,R0917,R0914,C0301  eiplgrader/ tests/

echo ""
echo "Running black code formatting check..."
black --check eiplgrader/ tests/

echo ""
echo "Running mypy type check..."
mypy --ignore-missing-imports eiplgrader/

echo ""
echo "Running pytest with coverage..."
python -m pytest --cov=eiplgrader/ tests/

echo ""
echo "All checks completed!"
