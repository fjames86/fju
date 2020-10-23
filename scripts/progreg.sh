#!/bin/sh

set -eu

freg put /fju/rpc/progreg key

CMDLIST=
PROGNAME=
PROGID=
CMDUNREG=
while getopts 'ln:ui:' c
do
    case $c in
	l) CMDLIST=y ;;
	n) PROGNAME=$OPTARG ;;
	u) CMDUNREG=y ;;
	i) PROGID=$OPTARG ;; 
    esac
done

if [ $CMDLIST ]; then
    freg /fju/rpc/progreg | tail -n +2 | awk '{print $3 " " $6}'     
elif [ $CMDUNREG ]; then
    if [ $PROGNAME ]; then
	freg rem /fju/rpc/progreg/$PROGNAME
    else
	echo "Need progname" 
	exit 1
    fi
else
    if [ $PROGNAME ]; then
    else
	freg /fju/rpc/progreg | tail -n +2 | awk '{print $3 " " $6}' 
	exit 0
    fi
    
    if [ $PROGID ]; then
    else
	PROGID=$(dd if=/dev/urandom bs=4 count=1 2> /dev/null | hd -v -e '"%d" "\n"' | head -n 2 | tail -n 1 )
	PROGID=$((20000000 + $PROGID % 20000000))
	
	while $(freg /fju/rpc/progreg | grep $PROGID); do
	    PROGID=$(dd if=/dev/urandom bs=4 count=1 2> /dev/null | hd -v -e '"%d" "\n"' | head -n 2 | tail -n 1 )
	    PROGID=$((20000000 + $PROGID % 20000000))
	done
    fi
    
    echo "$PROGNAME $PROGID" 
    freg put /fju/rpc/progreg/$PROGNAME u32 $PROGID
fi

	
