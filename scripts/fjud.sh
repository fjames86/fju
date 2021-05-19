#!/bin/sh

pidfile=/var/run/fjud.pid
port=$(fju reg get /fju/rpc/port || echo 8000)

cmd=$1
if [ ! $cmd ]; then
    cmd="status"
fi

if [ $cmd = "start" ]; then
    fju rpc fjud.stop > /dev/null
    sleep 0.1 
    fjud -u $port -t $port -p $pidfile
elif [ $cmd = "stop" ]; then
    fju rpc fjud.stop > /dev/null
    sleep 0.1 
    if [ -e $pidfile ]; then 
	rm -f $pidfile
    fi
elif [ $cmd = "status" ]; then
    fju rpc -p $port rpcbind.list
fi

