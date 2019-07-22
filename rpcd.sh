#!/bin/sh

cmd=$1
if [ $cmd = "start" ]; then
    if [ -e /etc/rpcd.pid ] && $(kill -0 $(cat /etc/rpcd.pid)); then
	echo "/etc/rpcd.pid exists and pid $(cat /etc/rpcd.pid) exists"
	exit 1
    fi
    
    bin/rpcd -R -u 8000 -p /etc/rpcd.pid 
elif [ $cmd = "stop" ]; then
    kill $(cat /etc/rpcd.pid)
    rm -f /etc/rpcd.pid 
fi

