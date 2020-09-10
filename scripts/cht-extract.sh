#!/bin/sh

set -eu

fingerprint=$1

for line in $(cat $fingerprint); do
    cht -r $line
done


    
