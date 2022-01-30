#!/bin/bash

regex="([0-9A-Z]*:)+"
DEVICES=$(bluetoothctl devices)
ICON=""
COLOR="#696B71"
for DEVICE in $DEVICES
do
    if [[ $DEVICE =~ $regex ]]; then
        STATUS=$(bluetoothctl info $DEVICE | grep "Connected" | awk '{print $2}')
        if [ $STATUS = "yes" ]; then
            ICON=""
            COLOR="#DFDFDF"
        fi
    fi
done

echo "<fc=$COLOR><fn=4>$ICON</fn></fc>"
