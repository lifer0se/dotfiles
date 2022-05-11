#!/bin/bash

if [ -z ${1+x} ]; then
  exit 1
fi

if [ "$2" = "mute" ]; then
  VALUE=""
elif [ -z ${2+x} ]; then
  VALUE="5"
else
  VALUE=$2
fi

ICONDIR="$HOME/downloads"

pulseaudio-ctl $1 $VALUE
MUTE=$(pulseaudio-ctl full-status | awk '{print $2}')
VOLUME=$(pulseaudio-ctl full-status | awk '{print $1}')

if [ "$VOLUME" -eq 0 ] || [ "$MUTE" = "yes" ]; then
    ICON="$ICONDIR/volume-mute.svg"
elif [ "$VOLUME" -lt 33 ]; then
    ICON="$ICONDIR/volume-low.svg"
elif [ "$VOLUME" -lt 77 ]; then
    ICON="$ICONDIR/volume-mid.svg"
else
    ICON="$ICONDIR/volume-full.svg"
fi

dunstify -i $ICON -u low -h string:x-dunst-stack-tag:volume "Volume: ${VOLUME}%" -h "int:value:${VOLUME}"
