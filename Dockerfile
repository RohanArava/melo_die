FROM ubuntu:22.04

# Install base dependencies
RUN apt-get update && apt-get install -y \
    lsb-release \
    wget \
    gnupg \
    software-properties-common \
    build-essential \
    cmake \
    clang \
    git \
    curl \
    nano

RUN curl https://sh.rustup.rs -sSf | bash -s -- -y 

# Download and install LLVM
RUN wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh && ./llvm.sh 18 all

RUN apt-get update && apt-get install -y \
    zlib1g-dev \
    libzstd-dev \
    libedit-dev \
    libcurl4-openssl-dev


# Set LLVM_DIR for CMake to find
ENV LLVM_DIR=/usr/lib/llvm-20/lib/cmake/llvm

ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /workspace
