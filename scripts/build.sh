#!/bin/sh

set -e

remoteip=$1
if [ ! $remoteip ]; then
    remoteip=$(fju reg get /remotehost)
fi

# backup registry in case something goes wrong! 
fju reg dump > /opt/fju/freg-backup.txt

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
scp bin/fju bin/fjud root@${remoteip}:/usr/local/bin
scp lib/libfju.so root@${remoteip}:/usr/local/lib
scp bin/nls.fvm root@${remoteip}:/root/nls.fvm
scp bin/log.fvm root@${remoteip}:/root/log.fvm
scp bin/cht.fvm root@${remoteip}:/root/cht.fvm
scp bin/dmb.fvm root@${remoteip}:/root/dmb.fvm


## restart remote services
sh scripts/fjud.sh start
ssh root@${remoteip} sh ~/fjud.sh start

# run the fvm test script
#sh scripts/test-fvm.sh

#tar czf fjud.tar.gz bin/fjud bin/rpclt bin/freg bin/fjlog
#cat scripts/install.sh fjud.tar.gz > bin/install.sh

