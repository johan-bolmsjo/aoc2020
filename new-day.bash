#!/bin/bash

if [ -x $1 ]; then
    echo "usage $0 DAY"
    exit 1
fi

DAY=$1
cp lib/day0.ml lib/day${DAY}.ml
cp lib/day0.mli lib/day${DAY}.mli
