

set -eu

freg put fju/fvm/modules/test-rpc key
freg put fju/fvm/modules/test-rpc/path str /root/fju/fvm/programs/test-rpc.fvm
freg put fju/fvm/programs/test-rpc str test-rpc
rpclt fvm.load filename=/root/fju/fvm/programs/test-rpc2.fvm register=true

echo "------------- TEST1 -------------------------- "
fvm fvm/test/test1.fvm
echo "------------- TEST2 -------------------------- "
fvm -m test2 -s MAIN fvm/test/test1.fvm fvm/test/test2.fvm
echo "------------- TEST3 -------------------------- "
fvm fvm/test/test3.fvm
echo "------------- TEST6 -------------------------- "
fvm fvm/test/test6.fvm

	
echo "------------- FVM.LIST -------------------------- "
rpclt fvm.list
echo "------------- test-rpc.fvm PROC-NULL ------------ "
rpclt raw 2333333 1 0
echo "------------- test-rpc.fvm PROC-HELLO ------------ "
rpclt raw 2333333 1 1
echo "------------- test-rpc.fvm PROC-ECHO ------------ "
rpclt raw 2333333 1 2 "str=echo me"
echo "------------- test-rpc.fvm PROC-COUNTER ------------ "
rpclt raw 2333333 1 3
rpclt raw 2333333 1 3
rpclt raw 2333333 1 3
echo "------------- test-rpc.fvm SET-MESSAGE ------------ "
rpclt raw 2333333 1 4 bool=true str=baz
echo "------------- test-rpc.fvm GET-MESSAGE ------------ "
rpclt raw 2333333 1 4 bool=false
echo "------------- done ------------------- ------------ "



