#!/bin/sh
N="$1"
shift
ocamlopt "./_$N/$N.ml" -o "./_$N/$N" && ./"_$N/$N" "$@"