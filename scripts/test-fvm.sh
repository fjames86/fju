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

echo "-------- TestSha1 ----------"

result=$(fju fvm -p TestHash --args $(fju xdr encode str=a) bin/test-sec.fvm | awk '{print $3}')
result=$(fju xdr decode 'xxxxx' $result | tr -d '\n')

# test sha1("a") gives correct result 
if [ "$result" = "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8" ]; then
    echo "Success"
else
    echo "Failure $result != $correct"
fi

echo "---------- LHT --------------"

fju fvm -p Put --args $(fju xdr encode str=testkey str=testval) bin/lht.fvm
echo "Should succeed:"
fju fvm -p Get --args $(fju xdr encode str=testkey) bin/lht.fvm
echo "Should fail:"
fju fvm -p Rem --args $(fju xdr encode str=testkey) bin/lht.fvm
fju fvm -p Get --args $(fju xdr encode str=testkey) bin/lht.fvm

echo "---------- Log ------------"

fju rpc raw 805271418 1 1 resb64 str=fju u32=0 u32=0 u32=100

echo "--------- Done -------------"



