#!/bin/sh

path=$1
hostid=$2
hshare=$3

nls add remote hshare=$hshare hostid=$hostid 
freg put /fju/cht/remote/$hshare str $path

logpath=/opt/fju/nls/$hostid/$hshare
fjlog -C CHT -p $logpath 

