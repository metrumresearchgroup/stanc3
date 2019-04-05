#Image used
FROM ubuntu:bionic

USER root

#Install dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates \
  rsync git build-essential m4 unzip pkg-config libpcre3-dev

RUN useradd -m opam
RUN groupadd opam-jenkins
RUN groupmod -g 1004 opam-jenkins
RUN usermod -aG opam,opam-jenkins opam

COPY ./scripts/install_ocaml.sh ./
RUN printf "\n" | bash -x install_ocaml.sh "--disable-sandboxing -y"

USER opam

RUN opam init --disable-sandboxing -y
RUN opam switch create 4.07.0
RUN opam switch 4.07.0
RUN eval $(opam env)

COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

COPY ./scripts/install_dev_deps.sh ./
RUN bash -x install_dev_deps.sh

ENTRYPOINT [ "opam", "config", "exec", "--" ]