#!/bin/sh

path=$1
logpath=$path.log 
fjlog -C CHT -p $logpath 

nls add share path=$logpath
hshare=$(nls | grep $logpath | awk '{print $2}')
freg put /fju/cht/local/$hshare str $path
cht -A $hshare -p $path

