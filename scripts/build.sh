#!/bin/sh

set -e

if [ -e bin/fju ]; then
    remoteip=$1
    if [ ! $remoteip ]; then
	remoteip=$(bin/fju reg get /remotehost)
    fi

    # backup registry in case something goes wrong! 
    bin/fju reg dump > /opt/fju/freg-backup.txt

    ## stop services and rebuild all
    bin/fju rpc fjud.stop 
fi

make clean all install

## stop remote service
scp scripts/fjud.sh root@${remoteip}:~
ssh root@${remoteip} sh ~/fjud.sh stop

## install remote
ssh root@${remoteip} mkdir -p /opt/fju
ssh root@${remoteip} mkdir -p /usr/local/bin
ssh root@${remoteip} mkdir -p /usr/local/lib
scp bin/fju bin/fjud root@${remoteip}:/usr/local/bin
#scp lib/libfju.so root@${remoteip}:/usr/local/lib
#scp bin/*.fvm root@${remoteip}:/root

## restart remote services
sh scripts/fjud.sh start
ssh root@${remoteip} sh ~/fjud.sh start

# run the fvm test script
#sh scripts/test-fvm.sh

#tar czf fjud.tar.gz bin/fjud bin/fjud lib/libfju.so
#cat scripts/install.sh fjud.tar.gz > bin/install.sh

