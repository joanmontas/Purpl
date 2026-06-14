FROM haskell:9.6-slim

USER root
RUN apt-get update && apt-get install -y \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

RUN curl -sSL https://github.com/tweag/ormolu/releases/download/0.8.1.0/ormolu-Linux.tar.gz | tar -xz -C /usr/local/bin/

WORKDIR /root/workspace

COPY cabal.project* Purpl.cabal ./

RUN cabal update && cabal build --only-dependencies --enable-tests

ENTRYPOINT ["cabal", "exec", "--"]
CMD ["bash"]