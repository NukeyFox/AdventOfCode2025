FROM mcr.microsoft.com/devcontainers/base:ubuntu

ENV DEBIAN_FRONTEND=noninteractive

# Install basic build dependencies
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    libffi-dev \
    libgmp-dev \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install ghcup and Haskell toolchain for the `vscode` user
USER vscode
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

ENV PATH=/home/vscode/.ghcup/bin:/home/vscode/.cabal/bin:$PATH

RUN /home/vscode/.ghcup/bin/ghcup install ghc 9.4.6 && \
    /home/vscode/.ghcup/bin/ghcup set ghc 9.4.6 && \
    /home/vscode/.ghcup/bin/ghcup install cabal 3.8.2.0 && \
    /home/vscode/.ghcup/bin/ghcup set cabal 3.8.2.0 && \
    /home/vscode/.ghcup/bin/ghcup install hls 1.7.0 || true

RUN /home/vscode/.cabal/bin/cabal update || true

USER vscode
