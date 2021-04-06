#!/bin/sh

echo "----- Hello World ----------"

fju fvm fvm/test/hello-world.fvm

echo "------- Test ---------------"

fju fvm fvm/test/test.fvm

echo "----- TestDmb --------------"

fju reg rem /dmbtest > /dev/null 

fju rpc fvm.load filename=fvm/test/test-dmb.fvm reload > /dev/null
fju rpc hemlock fvm.load filename=fvm/test/test-dmb.fvm reload > /dev/null
fju rpc fvm.run modname=DmbTest procname=TestMsg > /dev/null
sleep 1

fju reg /dmbtest

echo "----- TestDmb2 --------------"

fju rpc fvm.run modname=DmbTest procname=TestMsg2

echo "--------- Done -------------"



