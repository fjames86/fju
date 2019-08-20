#!/bin/sh

## Generate an freg merge file using the dump command.
## Save the output from that to a file and pass that file
## to this script to merge those values in.
## This procedure can be used for either passing values to
## other hosts or for backup+restore. 

regfile=$1

while IFS= read -r LINE; do
    bin/freg put $LINE
done < $regfile

