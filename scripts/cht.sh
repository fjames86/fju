#!/bin/sh

## Write to cht across cluster
## Usage: cht.sh key datab64
## E.g. cht.sh $(xdru encode u32=123 u32=321 u32=123 u32=123) $(xdru encode str=hello)

# get cluster id for fvm cluster 
clid=$(raft | grep 805271413 | awk '{print $1}')
key=$1 
databuf=$2

$keyparts = $(xdru decode $key u32 u32 u32 u32)
$keyb64 = $(xdru encod u32=$(echo $keyparts | awk '{print $1}') u32=$(echo $keyparts | awk '{print $2}') u32=$(echo $keyparts | awk '{print $3}') u32=$(echo $keyparts | awk '{print $4}'))

# encode command to run the fju module ChtWrite procedure 
procargs=$(xdru encode opaque=$keyb64 opaque=$databuf)
command=$(xdru encode str="FJU" u32=1 u64=0 str="ChtWrite" opaque=$procargs)

#echo "rpclt raft.command clid=$clid command=$command"
rpclt raft.command clid=$clid command=$command

