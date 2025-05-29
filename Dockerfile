FROM ubuntu:24.04

# Note that the system_rust_toolchain assume that "rustc" is available in the PATH
ENV DEBIAN_FRONTEND=noninteractive \
    PATH=/root/.cargo/bin:$PATH

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        build-essential \
        gpg \
        git \
        curl \
        wget \
        unzip \
        libssl-dev \
        libzstd-dev \
        python3 \
        clang \
      && rm -rf /var/lib/apt/lists/*

# The rust toolchain version in the container should be consistent with
# the one declared in //:rust-toolchain file.
COPY rust-toolchain /tmp/rust-toolchain
RUN set -e; \
    RUST_CHANNEL="$(grep -E '^[[:space:]]*channel' /tmp/rust-toolchain \
                     | head -1 \
                     | cut -d'=' -f2 \
                     | tr -d ' \"')" && \
    echo "Installing Rust toolchain ${RUST_CHANNEL}" && \
    curl -sSf https://sh.rustup.rs \
      | sh -s -- -y --profile minimal --default-toolchain "${RUST_CHANNEL}" && \
    rustup component add clippy

CMD [ "bash" ]
