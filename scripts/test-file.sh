#!/bin/sh
set -eu

if [ $# -ne 1 ]; then
  echo "Usage: test-file [FILE]"
  exit 1
fi

opam config exec -- jbuilder build src/main.exe
opam config exec -- jbuilder build src/cohttp_server.exe

_build/default/src/cohttp_server.exe &
pid="$!"

_build/default/src/main.exe -v $1

kill -15 pid
