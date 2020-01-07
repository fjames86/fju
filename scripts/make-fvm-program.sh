#!/bin/sh

prog=$1
sbcl --noinform --core fvm.core --non-interactive --load fvm/programs/${prog}.lisp --eval "(${prog}:${prog})"
mv ${prog}.fvm bin/

