# FROM fpco/stack-build:lts-21.1 # クラッシュするので断念, apple m1がダメ？
FROM debian:buster-slim as build

# install stack
RUN apt-get update && apt-get install -y curl
ENV PATH ~/.local/bin:$PATH
RUN curl -sSL https://get.haskellstack.org/ | sh

# build
WORKDIR /app
COPY source/ /app/
RUN stack setup --system-ghc
RUN stack build --system-ghc

CMD ["stack", "run"]
