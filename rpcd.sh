#!/bin/sh

pidfile=/var/run/rpcd.pid
udpport=8000

cmd=$1
if [ $cmd = "start" ]; then
    if [ -e $pidfile ] && $(kill -0 $(cat $pidfile)); then
	echo "$pidfile exists and pid $(cat $pidfile) exists - restarting"
	kill $(cat $pidfile)
	sleep 1 
    fi
    
    bin/rpcd -u $udpport -p $pidfile
elif [ $cmd = "stop" ]; then
    if [ -e $pidfile ]; then 
	kill $(cat $pidfile)
	rm -f $pidfile
    fi
elif [ $cmd = "status" ]; then
    if [ -e $pidfile ]; then
	bin/rpcinfo -p $udpport
    else
	echo "rpcd not running"
    fi
fi

