#!/bin/sh

## stop services and rebuild all 
sh scripts/rpcd.sh stop 
make clean all install

## stop remote service
scp scripts/rpcd.sh root@169.254.54.129:~
ssh root@169.254.54.129 sh ~/rpcd.sh stop

## install remote
ssh root@169.254.54.129 mkdir -p /opt/fju
ssh root@169.254.54.129 mkdir -p /usr/local/bin
ssh root@169.254.54.129 mkdir -p /usr/local/lib
scp bin/* root@169.254.54.129:/usr/local/bin
scp lib/libfju.so root@169.254.54.129:/usr/local/lib
scp lib/librex.so root@169.254.54.129:~

## restart remote services
sh scripts/rpcd.sh start
ssh root@169.254.54.129 sh ~/rpcd.sh start





