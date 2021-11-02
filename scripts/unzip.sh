#!/bin/sh

ZIP=$1
DIR="${ZIP%.*}"
[[ ! -f $ZIP ]] || [[ "${ZIP##*.}" != "zip" ]] && echo "Please provide the correct path to a .zip" && exit
7z x -o$DIR $ZIP
