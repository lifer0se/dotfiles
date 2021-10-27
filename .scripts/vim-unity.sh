#!/bin/sh

TERM="termite"
SERVER="/tmp/unity"

if [ ! -z `nvr --serverlist | grep $SERVER` ]; then
    nvr --servername $SERVER +$2 "$1"
else
    FILE=$(echo $1 | sed 's! !\\ !g')
    $TERM -e "nvr --servername $SERVER +$2 $FILE"
fi
