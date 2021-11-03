#!/bin/sh

LANG=`setxkbmap -query | awk '/layout/{print $2}'`
echo "$LANG"
