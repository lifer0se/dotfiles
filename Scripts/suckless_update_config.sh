#!/bin/sh
BUILDPATH=~/.gitclones/$1
[[ ! $1 ]] && BUILDPATH=~/.gitclones/dwm
[[ ! -d $BUILDPATH ]] && echo "\"$1\" does not exists in ~/.gitclones." && exit
cd $BUILDPATH
sudo cp config.def.h config.h
sudo make CDBG=-w clean install
