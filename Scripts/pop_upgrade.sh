#!/bin/sh

printf "Beginning upgrade.\\n"

paru -Syu

NUM=$(pacman -Qu | grep -Fcv "[ignored]")
echo $NUM > ~/.local/share/.pacman_updates_count

printf "\\nUpgrade complete.\\nPress <Enter> to exit window.\\n\\n"
read -r _
