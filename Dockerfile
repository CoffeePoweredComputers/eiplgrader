# Multi-stage build for minimal final image size
FROM python:3.13-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    gcc \
    g++ \
    musl-dev \
    libffi-dev \
    openssl-dev

# Create wheels for Python dependencies
WORKDIR /build
COPY requirements-docker.txt .
RUN pip wheel --no-cache-dir --no-deps --wheel-dir /wheels -r requirements-docker.txt

# Final stage - minimal runtime image
FROM python:3.13-alpine

# Install runtime dependencies and language toolchains
RUN apk add --no-cache \
    # C/C++ runtime
    gcc \
    g++ \
    musl-dev \
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

# Copy wheels from builder
COPY --from=builder /wheels /wheels

# Install Python dependencies from wheels
RUN pip install --no-cache-dir --no-index --find-links=/wheels /wheels/* && \
    rm -rf /wheels

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