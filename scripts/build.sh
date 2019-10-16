#!/bin/sh

remoteip=$1
if [ ! $remoteip ]; then
    remoteip=$(freg get /remotehost)
fi

## stop services and rebuild all 
sh scripts/fjud.sh stop 
make clean all install

## stop remote service
scp scripts/fjud.sh root@${remoteip}:~
ssh root@${remoteip} sh ~/fjud.sh stop

## install remote
ssh root@${remoteip} mkdir -p /opt/fju
ssh root@${remoteip} mkdir -p /usr/local/bin
ssh root@${remoteip} mkdir -p /usr/local/lib
scp bin/* root@${remoteip}:/usr/local/bin
scp lib/libfju.so root@${remoteip}:/usr/local/lib
scp lib/librex.so root@${remoteip}:~

## restart remote services
sh scripts/fjud.sh start
ssh root@${remoteip} sh ~/fjud.sh start





