#!/bin/sh

LISTPATH="$0"
LISTPATH=${LISTPATH%/*}/iconlists/
[[ ! -d $LISTPATH ]] && echo "No icon lists found." && exit
cat $LISTPATH$(ls $LISTPATH | sed 's,\.list,,' | dmenu -l 20)".list" | dmenu -l 20 | awk '{print $1}' | tr -d '\n' | xclip -selection clipboard

ICON=$(xclip -o -selection clipboard)
LEN=${#ICON}
if [ $LEN -gt 0 ]
then
    notify-send "Copied to clipboard:   $ICON   "
fi
