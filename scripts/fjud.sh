#!/bin/sh

pidfile=/var/run/fjud.pid
port=$(freg get /fju/rpc/port || echo 8000)

cmd=$1
if [ ! $cmd ]; then
    cmd="status"
fi

if [ $cmd = "start" ]; then
    rpclt fjud.stop > /dev/null
    sleep 0.1 
    fjud -u $port -t $port -p $pidfile
elif [ $cmd = "stop" ]; then
    rpclt fjud.stop > /dev/null
    sleep 0.1 
    if [ -e $pidfile ]; then 
	rm -f $pidfile
    fi
elif [ $cmd = "status" ]; then
    rpclt -p $port rpcbind.list
fi

