#!/bin/sh

echo "----- Hello World ----------"

fju fvm -m bin/hello-world.fvm 

echo "------- Test ---------------"

fju fvm -m bin/test.fvm 

echo "----- TestDmb --------------"

fju reg rem /dmbtest > /dev/null 

fju rpc fvm.load filename=bin/test-dmb.fvm reload > /dev/null
fju rpc hemlock fvm.load filename=bin/test-dmb.fvm reload > /dev/null
fju rpc fvm.run DmbTest/TestMsg > /dev/null
sleep 1

echo -n "Result: "
fju reg /dmbtest

echo "----- TestDmb2 --------------"

fju rpc fvm.run DmbTest/TestMsg2

echo "------- TestFile ------------"

if [ -e /opt/fju/test.txt ]; then
    rm /opt/fju/test.txt
fi

fju fvm -m bin/test-file.fvm TestFile/TestProc test.txt
if [ -e /opt/fju/test.txt ]; then
    echo "Success"
else
    echo "Failure"
fi
rm /opt/fju/test.txt

echo "-------- TestSha1 ----------"

result=$(fju fvm -m bin/test-sec.fvm --args $(fju xdr encode str=a) TestSec/TestHash | awk '{print $3}')
result=$(fju xdr decode 'xxxxx' $result | tr -d '\n')

# test sha1("a") gives correct result
correct="86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"
if [ "$result" = "$correct" ]; then
    echo "Success"
else
    echo "Failure $result != $correct"
fi

echo "---------- LHT --------------"

fju fvm -m bin/lht.fvm Lht/Put $(fju xdr encode str=testkey) $(fju xdr encode str=testval)
echo "Should succeed:"
fju fvm -m bin/lht.fvm Lht/Get $(fju xdr encode str=testkey) 
echo "Should fail:"
fju fvm -m bin/lht.fvm Lht/Rem $(fju xdr encode str=testkey)
fju fvm -m bin/lht.fvm Lht/Get $(fju xdr encode str=testkey) 

echo "---------- Log ------------"

fju rpc raw 805271418 1 1 resb64 str=fju u32=0 u32=0 u32=100

echo "--------- Done -------------"



