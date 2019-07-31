#!/bin/sh

cmd=$1
if [ $cmd = "start" ]; then
    if [ -e /etc/rpcd.pid ] && $(kill -0 $(cat /etc/rpcd.pid)); then
	echo "/etc/rpcd.pid exists and pid $(cat /etc/rpcd.pid) exists - restarting"
	kill $(cat /etc/rpcd.pid)
	sleep 1 
    fi
    
    bin/rpcd -R -u 8000 -p /etc/rpcd.pid 
elif [ $cmd = "stop" ]; then
    if [ -e /etc/rpcd.pid ]; then 
	kill $(cat /etc/rpcd.pid)
	rm -f /etc/rpcd.pid
    fi
elif [ $cmd = "status" ]; then
    if [ -e /etc/rpcd.pid ]; then
	bin/rpcinfo -p 8000
    else
	echo "rpcd not running"
    fi
fi

