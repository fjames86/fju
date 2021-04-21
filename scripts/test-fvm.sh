#!/bin/sh

echo "----- Hello World ----------"

fju fvm bin/hello-world.fvm

echo "------- Test ---------------"

fju fvm bin/test.fvm

echo "----- TestDmb --------------"

fju reg rem /dmbtest > /dev/null 

fju rpc fvm.load filename=bin/test-dmb.fvm reload > /dev/null
fju rpc hemlock fvm.load filename=bin/test-dmb.fvm reload > /dev/null
fju rpc fvm.run modname=DmbTest procname=TestMsg > /dev/null
sleep 1

echo -n "Result: "
fju reg /dmbtest

echo "----- TestDmb2 --------------"

fju rpc fvm.run modname=DmbTest procname=TestMsg2

echo "------- TestFile ------------"

if [ -e /opt/fju/test.txt ]; then
    rm /opt/fju/test.txt
fi

fju fvm -p TestProc --args $(fju xdr encode str=test.txt) bin/test-file.fvm
if [ -e /opt/fju/test.txt ]; then
    echo "Success"
else
    echo "Failure"
fi
rm /opt/fju/test.txt

echo "--------- Done -------------"



