
modpath=$1

# get module name by disassembling module binary 
progname=$(fvmc -d $modpath | head -n 2 | grep NAME | awk '{print $2}')

# optional cluster 
clusterid=$2

# if not set, try and get default cluster 
if [ ! $clusterid ]; then
    clusterid=$(freg -q get /fju/raft/clusterid || echo -n "")
fi

# write registry values
freg put /fju/fvm key
freg put /fju/fvm/modules key 
freg put /fju/fvm/modules/$progname key
freg put /fju/fvm/modules/$progname/path str $modpath
if [ $clusterid ]; then
    freg put /fju/fvm/modules/$progname/cluster u64 $clusterid
fi
freg put /fju/fvm/programs/$progname str $progname
freg put /fju/fvm/service key
freg put /fju/fvm/service/$progname
