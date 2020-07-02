
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



