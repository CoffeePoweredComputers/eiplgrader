# Single-stage build for reliability
FROM python:3.13-alpine

# Install build and runtime dependencies and language toolchains
RUN apk add --no-cache \
    # Build dependencies
    gcc \
    g++ \
    musl-dev \
    libffi-dev \
    openssl-dev \
    # Java runtime
    openjdk17-jre \
    # Node.js for JavaScript
    nodejs \
    # Go compiler
    go \
    # Haskell compiler
    ghc \
    # Required for some Python packages
    libffi \
    openssl \
    # Minimal shell utilities
    bash

# Install Python dependencies directly
COPY requirements-docker.txt .
RUN pip install --no-cache-dir -r requirements-docker.txt && \
    rm requirements-docker.txt

# Create non-root user
RUN adduser -D -u 1000 grader

# Create working directory
WORKDIR /app

# Copy application code
COPY eiplgrader/ ./eiplgrader/
COPY docker_entrypoint.py .

# Set ownership
RUN chown -R grader:grader /app

# Create temporary directory for code execution
RUN mkdir -p /tmp/grader && \
    chown grader:grader /tmp/grader

# Switch to non-root user
USER grader

# Set environment variables
ENV PYTHONUNBUFFERED=1
ENV PYTHONDONTWRITEBYTECODE=1
ENV TMPDIR=/tmp/grader

# Entry point
ENTRYPOINT ["python", "docker_entrypoint.py"]