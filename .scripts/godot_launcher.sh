#!/bin/sh

MOUSE=$(xdotool getmouselocation | awk '{print $1}' | sed 's/x://')

if [[ $MOUSE -gt 1920 ]]
then
    xdotool key Super+6
    xdotool key Super+o
    xdotool key Super+6
else
    xdotool key Super+o
    xdotool key Super+6
    xdotool key Super+o
    xdotool key Super+6
fi

godot &

xdotool key Super+5

termite -e "nvr --servername /tmp/godot"
