#!/bin/sh

PACS=$(pacman -Qu | grep -Fcv "[ignored]")
if [ "$PACS" -gt "0" ] ; then
    echo "<fn=1></fn>  <fc=#AAC0F0>$PACS</fc>   |"
fi
