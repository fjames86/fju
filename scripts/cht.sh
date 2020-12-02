#!/bin/sh

## Write to cht across cluster
## Usage: cht.sh key datab64
## E.g. cht.sh $(xdru encode u32=123 u32=321 u32=123 u32=123) $(xdru encode str=hello)

# get cluster id for fvm cluster 
clid=$(raft | grep 805271413 | awk '{print $1}')
key=$1 
databuf=$2

xdru decode $key u32 u32 u32 u32 > /dev/null
if [ ! $? -eq 0 ]; then
    echo "Bad key length - must be 16 "
    exit 1
fi

# encode command to run the fju module (progid=10000) ChtWrite procedure (procid=3)
# encode: progid=10000 mode=1 hostid=0 procid=3 args=opaque
procargs=$(xdru encode fixed=$key opaque=$databuf)
command=$(xdru encode u32=10000 u32=1 u64=0 u32=3 opaque=$procargs)

#echo "rpclt raft.command clid=$clid command=$command"
rpclt raft.command clid=$clid command=$command

