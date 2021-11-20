#!/bin/sh

PACS=$(cat ~/.local/share/.pacman_updates_count)
if [ "$PACS" -gt "0" ] ; then
    echo "<fn=1></fn>  <fc=#AAC0F0>$PACS</fc>     |"
fi
