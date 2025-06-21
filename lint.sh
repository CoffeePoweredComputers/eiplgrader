#!/bin/bash
# Lint script for eiplgrader
# Runs all code quality checks from CI workflow locally

# Track failed stages
failed_stages=()
exit_code=0

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
if pylint eiplgrader/ tests/; then
    echo "✓ pylint passed"
else
    echo "✗ pylint failed"
    failed_stages+=("pylint")
    exit_code=1
fi

echo ""
echo "Running black code formatting check..."
if black --check eiplgrader/ tests/; then
    echo "✓ black formatting check passed"
else
    echo "✗ black formatting check failed"
    failed_stages+=("black")
    exit_code=1
fi

echo ""
echo "Running mypy type check..."
if mypy --ignore-missing-imports eiplgrader/; then
    echo "✓ mypy type check passed"
else
    echo "✗ mypy type check failed"
    failed_stages+=("mypy")
    exit_code=1
fi

#echo ""
#echo "Running pytest with coverage..."
#if python -m pytest --cov=eiplgrader/ tests/; then
#    echo "✓ pytest with coverage passed"
#else
#    echo "✗ pytest with coverage failed"
#    failed_stages+=("pytest")
#    exit_code=1
#fi

echo ""
echo "========== Summary =========="
if [ ${#failed_stages[@]} -eq 0 ]; then
    echo "✓ All checks passed!"
else
    echo "✗ Failed stages: ${failed_stages[*]}"
    echo "Please fix the issues above and run again."
fi

exit $exit_code
