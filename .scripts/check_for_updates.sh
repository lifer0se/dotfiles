#!/bin/sh

ping -q -c 1 example.org > /dev/null || exit

! [[ $(echo $DBUS_SESSION_BUS_ADDRESS) ]] && export DBUS_SESSION_BUS_ADDRESS=$(grep -z DBUS_SESSION_BUS_ADDRESS /proc/$(pgrep -u $LOGNAME session)/environ | cut -d= -f2-)

sudo pacman -Syyuw --noconfirm || notify-send "Error downloading updates." "Check your internet connection, if pacman is already running, or run update manually to see errors."
# pkill -RTMIN+8 "${STATUSBAR:-dwmblocks}"


OLDNUM=$( [[ -f ~/.local/share/.pacman_updates_count ]] && cat ~/.local/share/.pacman_updates_count)
NUM=$(pacman -Qu | grep -Fcv "[ignored]")
echo $NUM > ~/.local/share/.pacman_updates_count
[[ $NUM -eq 0 ]] && exit

if ! [[ "$OLDNUM" =~ ^[0-9]+$ ]] || [[ $OLDNUM -gt $NUM ]]
then
    echo $NUM > ~/.local/share/.pacman_updates_count
    OLDNUM=0
fi

ICON=/usr/share/icons/Papirus-Dark/64x64/apps/mx-packageinstaller.svg
NEWNUM=$(($NUM - $OLDNUM))
if [[ $NEWNUM -gt 1 ]]
then
	notify-send -i $ICON "Repository Sync" "$NEWNUM new updates available."
elif [[ $NEWNUM -gt 0 ]]
then
	notify-send -i $ICON "Repository Sync" "$NEWNUM new update available."
fi
