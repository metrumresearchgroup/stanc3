#Image used
FROM ubuntu:bionic

RUN useradd -m opam
RUN usermod -aG sudo opam

USER root

#Install dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates \
  rsync git build-essential m4 unzip pkg-config libpcre3-dev sudo

RUN echo "opam ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

USER opam

COPY ./scripts/install_ocaml.sh ./
RUN printf "\n" | bash -x install_ocaml.sh "--disable-sandboxing -y"

COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

COPY ./scripts/install_dev_deps.sh ./
RUN bash -x install_dev_deps.sh

ENTRYPOINT [ "opam", "config", "exec", "--" ]