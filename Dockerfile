FROM gitpod/workspace-full

USER root

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN echo "deb http://archive.canonical.com/ubuntu cosmic partner" >> /etc/apt/source.list
RUN apt update
RUN apt install -y llvm-7 llvm-7-dev clang-7 apt-utils
