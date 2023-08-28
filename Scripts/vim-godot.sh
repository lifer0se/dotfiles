#!/bin/sh

TERM="alacritty"
SERVER="nvim.godot"
FILE=$1
LINE=$2
COL=$3

if [ ! -z `ls /tmp/ | grep $SERVER` ]; then
	nvim --server /tmp/$SERVER --remote-send "<esc>:n $FILE<CR>:call cursor($LINE,$COL)<CR>"
else
	$TERM -e nvim --listen /tmp/$SERVER "+find $FILE" "+call cursor($LINE,$COL)"
fi
