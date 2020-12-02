#!/bin/sh

set -e

## Write to a log across the whole cluster.
## Usage: nls.sh logname base64message
## E.g. nls.sh mylog `xdru encode str=hello`
##

# get cluster id for fvm cluster 
clid=$(raft | grep 805271413 | awk '{print $1}')
logname=$1
logbuf=$2

# encode command to run the fju module (progid=10000) LogWrite procedure (procid=2)
# this procedure takes two args: a string naming the log and an opaque buffer to write
# encode: progid=10000 mode=1 hostid=0 procid=2 args=opaque 
procargs=$(xdru encode str=$logname opaque=$logbuf)
command=$(xdru encode u32=10000 u32=1 u64=0 u32=2 opaque=$procargs)

#echo "rpclt raft.command clid=$clid command=$command"
rpclt raft.command clid=$clid command=$command
