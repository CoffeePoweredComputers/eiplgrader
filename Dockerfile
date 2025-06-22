FROM python:3.13-slim

# Install supported language toolchains for sandboxed execution
RUN apt-get update && apt-get install -y --no-install-recommends \
    # C/C++ compilers
    gcc g++ \
    # Go
    golang \
    # Haskell
    ghc \
    # Java
    default-jdk \
    # JavaScript (Node.js)
    nodejs npm \
    # Tools for sandboxing
    curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user for security
RUN useradd -m -u 1000 sandboxed
USER sandboxed

# Set working directory
WORKDIR /home/sandboxed/app

# Install eiplgrader
RUN pip install --user eiplgrader

# Set PATH to include user-installed packages
ENV PATH=/home/sandboxed/.local/bin:$PATH
ENV PYTHONUNBUFFERED=1

# Default command to run eiplgrader
CMD ["python", "-c", "import eiplgrader; print('eiplgrader ready for sandboxed execution')"]
