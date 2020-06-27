#!/bin/sh

pidfile=/var/run/fjud.pid
udpport=$(freg get /fju/rpc/port || echo 8000)

cmd=$1
if [ ! $cmd ]; then
    cmd="status"
fi

if [ $cmd = "start" ]; then
    if [ -e $pidfile ] && $(kill -0 $(cat $pidfile)); then
	echo "$pidfile exists and pid $(cat $pidfile) exists - restarting"
	kill $(cat $pidfile)
	sleep 1 
    fi
    
    fjud -u $udpport -p $pidfile
elif [ $cmd = "stop" ]; then
    rpclt fjud.stop > /dev/null
    if [ -e $pidfile ]; then 
	kill $(cat $pidfile)
	rm -f $pidfile
    fi
elif [ $cmd = "status" ]; then
    if [ -e $pidfile ]; then
	rpclt -p $udpport rpcbind.list
    else
	echo "fjud not running"
    fi
fi

