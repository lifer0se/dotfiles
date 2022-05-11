#!/bin/sh

TERM="alacritty"
SERVER="/tmp/godot"

FILE=$(echo $1 | sed 's! !\\ !g')
LINE=$2
let LINE+=1
if [ ! -z `nvr --serverlist | grep $SERVER` ]; then
  nvr --servername $SERVER +$LINE $FILE
else
  $TERM -e nvr --servername $SERVER +$LINE $FILE
fi
