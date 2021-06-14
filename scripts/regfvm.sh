
# get module name by disassembling module binary 

modpath=""
serviceproc=""
initproc=""
regprog=""
regtext=""

while getopts 'p:s:i:rt' c
do
    case $c in
	p) modpath=$OPTARG ;;
	s) serviceproc=$OPTARG ;;
	i) initproc=$OPTARG ;;
	r) regprog="y" ;;
	t) regtext="y" ;;
    esac
done

if [ ! $modpath ]; then
    echo "Need module path"
    exit 1
fi

progname=$(fju fvmc -d $modpath | head -n 2 | grep -i NAME | awk '{print $2}')
if [ ! $progname ]; then
    echo "$modpath Need progname"
    exit 1
fi

# install module
fju reg put /fju/fvm key
fju reg put /fju/fvm/modules key 
fju reg put /fju/fvm/modules/$progname key
if [ $regtext ]; then
    fju reg put /fju/fvm/modules/$progname/text opaque-file $modpath
else
    fju reg put /fju/fvm/modules/$progname/path str $modpath
fi

# set init routine
if [ $initproc ]; then
    fju reg put /fju/fvm/modules/$progname/init str $initproc
else
    fju reg rem /fju/fvm/modules/$progname/init > /dev/null
fi

# set service proc 
if [ $serviceproc ]; then
    fju reg put /fju/fvm/modules/$progname/service str $serviceproc
else
    fju reg rem /fju/fvm/modules/$progname/service > /dev/null
fi

# register as rpc program
if [ $regprog ]; then
    fju reg put /fju/fvm/modules/$progname/register u32 1
fi

