#Use Official Docker Images for OCAML/OPAM
#https://github.com/ocaml/infrastructure/wiki/Containers
FROM ocaml/opam2:debian-stable

USER root

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates \
  rsync git build-essential m4 unzip pkg-config libpcre3-dev

USER opam

#Copy & Run the script that installs the opam build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

#Copy & Run the script that installs the opam development dependencies
COPY ./scripts/install_dev_deps.sh ./
RUN bash -x install_dev_deps.sh

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]