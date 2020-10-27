#!/bin/sh

# xx/rpc?prog=1231&vers=1&proc=123&args=xxxbase64xxx
echo "Content-type: text\n\n";

PROG=0
VERS=0
PROC=0
ARGS=""

for par in $(echo $QUERY_STRING | tr '&' "\n"); do
    pname=$(echo $par | cut -d '=' -f 1)
    if [ $pname = "prog" ]; then
	PROG=$(echo $par | cut -d '=' -f 2)
    elif [ $pname = "vers" ]; then
	VERS=$(echo $par | cut -d '=' -f 2)
    elif [ $pname = "proc" ]; then
	PROC=$(echo $par | cut -d '=' -f 2)
    elif [ $pname = "args" ]; then
	ARGS=$(echo $par | sed 's/args=//')
    fi
done

rpclt local raw $PROG $VERS $PROC fixed=$ARGS resb64

