#!/bin/sh

TERM="termite"
SERVER="GODOT"

if [ ! -z `vim --serverlist | grep $SERVER` ]; then
    vim --servername $SERVER --remote-silent-tab +$2 "$1"
else
    FILE=$(echo $1 | sed 's! !\\ !g')
    $TERM -e "vim --servername $SERVER +$2 $FILE"
fi
