#Image used
FROM ubuntu:bionic

USER root

#Install dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates \
  rsync git build-essential m4 unzip pkg-config libpcre3-dev sudo python

RUN useradd -m opam_ec2
RUN usermod -aG sudo opam_ec2

RUN useradd -m opam_jenkins
RUN usermod -aG sudo opam_jenkins

RUN echo "opam_ec2 ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN echo "opam_jenkins ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

RUN usermod -u 1000 opam_ec2
RUN usermod -u 1004 opam_jenkins

COPY ./scripts/install_ocaml.sh ./
COPY ./scripts/install_build_deps.sh ./
COPY ./scripts/install_dev_deps.sh ./

USER opam_jenkins

RUN printf "\n" | bash -x install_ocaml.sh "--disable-sandboxing -y"
RUN bash -x install_build_deps.sh
RUN bash -x install_dev_deps.sh

USER opam_ec2

RUN printf "\n" | bash -x install_ocaml.sh "--disable-sandboxing -y"
RUN bash -x install_build_deps.sh
RUN bash -x install_dev_deps.sh

ENTRYPOINT [ "opam", "config", "exec", "--" ]