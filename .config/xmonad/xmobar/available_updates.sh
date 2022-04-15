#!/bin/sh

PACS=$(cat ~/.local/share/.pacman_updates_count)
if [ "$PACS" -gt "0" ] ; then
    echo "<fc=#AAC0F0>$PACS</fc>  <fn=1></fn>     |"
fi
