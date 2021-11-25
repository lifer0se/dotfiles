#!/bin/sh

export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
ICON=/usr/share/icons/Papirus-Dark/64x64/apps/mx-packageinstaller.svg

ping -q -c 1 example.org > /dev/null || exit
sudo pacman -Syyuw --noconfirm || notify-send -i $ICON "Error downloading updates." "Check your internet connection, if pacman is already running, or run update manually to see errors."

OLDNUM=$( [[ -f ~/.local/share/.pacman_updates_count ]] && cat ~/.local/share/.pacman_updates_count)
NUM=$(pacman -Qu | grep -Fcv "[ignored]")
echo $NUM > ~/.local/share/.pacman_updates_count
[[ $NUM -eq 0 ]] && exit

if ! [[ "$OLDNUM" =~ ^[0-9]+$ ]] || [[ $OLDNUM -gt $NUM ]]
then
    echo $NUM > ~/.local/share/.pacman_updates_count
    OLDNUM=0
fi

NEWNUM=$(($NUM - $OLDNUM))
if [[ $NEWNUM -gt 0 ]]
then
  if [[ $NUM -gt 1 ]]
  then
    notify-send -i $ICON "Repository Sync" "$NUM updates available."
  else
    notify-send -i $ICON "Repository Sync" "$NUM update available."
  fi
fi
