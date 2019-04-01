#Use Official Docker Images for OCAML/OPAM
#https://github.com/ocaml/infrastructure/wiki/Containers
FROM ocaml/opam2:alpine

#Switch to root user so we can install apk packages
USER root

#Set our distro_style
LABEL distro_style="apk"

#Install os dependencies
RUN apk update && apk upgrade && \
  apk add build-base bzip2 git tar curl ca-certificates openssl \
  patch rsync bash libx11-dev nano bubblewrap coreutils xz ncurses-dev m4 pcre-dev

#Switch back to the normal user
USER opam

RUN opam init -y --disable-sandboxing -y
RUN opam switch create 4.07 ocaml-base-compiler.4.07.1 && \
  opam switch 4.07

#Copy script & Install build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]