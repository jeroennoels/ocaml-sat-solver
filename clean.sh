#!/bin/bash
find ../ocaml-sat-solver/ -name "*~"   -print0 | xargs -r -0 /bin/rm
