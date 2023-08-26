# FROM fpco/stack-build:lts-21.1 # クラッシュするので断念, apple m1がダメ？
# FROM debian:buster-slim as build
FROM haskell:9.2.8-slim-buster

# install stack
# RUN apt-get update
# RUN apt-get install -y ghc

# build
WORKDIR /app
RUN stack --system-ghc install servant-server lens conduit skylighting --resolver lts-20.26
COPY source/ /app/

RUN stack --system-ghc build --dependencies-only
RUN stack --system-ghc install
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/bin

CMD ["/opt/bin/nobanashi"]
