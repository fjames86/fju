
progname=$1
modpath=$2
clusterid=$3

freg put /fju/fvm/modules/$progname key
freg put /fju/fvm/modules/$progname/path str $modpath
freg put /fju/fvm/modules/$progname/cluster u64 $clusterid 
freg put /fju/fvm/programs/$progname str $progname
freg put /fju/fvm/service key
freg put /fju/fvm/service/$progname
