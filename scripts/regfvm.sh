
# get module name by disassembling module binary 

modpath=""
serviceproc=""
initproc=""
regprog=""

while getopts 'p:s:i:r' c
do
    case $c in
	p) modpath=$OPTARG ;;
	s) serviceproc=$OPTARG ;;
	i) initproc=$OPTARG ;;
	r) regprog="y" ;;
    esac
done

if [ ! $modpath ]; then
    echo "Need module path"
    exit 1
fi

progname=$(fvmc -d $modpath | head -n 2 | grep NAME | awk '{print $2}')
if [ ! $progname ]; then
    echo "Need progname"
    exit 1
fi

# install module
freg put /fju/fvm key
freg put /fju/fvm/modules key 
freg put /fju/fvm/modules/$progname key
freg put /fju/fvm/modules/$progname/path str $modpath

# set init routine
if [ $initproc ]; then
    freg put /fju/fvm/modules/$progname/init str $initproc
else
    freg rem /fju/fvm/modules/$progname/init > /dev/null
fi

# set service proc 
if [ $serviceproc ]; then
    freg put /fju/fvm/modules/$progname/service str $serviceproc
else
    freg rem /fju/fvm/modules/$progname/service > /dev/null
fi

# register as rpc program
if [ $regprog ]; then
    freg put /fju/fvm/modules/$progname/register u32 1
else
    freg put /fju/fvm/modules/$progname/register u32 0    
fi

