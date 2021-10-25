#!/bin/sh

temp1=70
temp2=85

temp=$(sensors | grep 'Package id 0:' | awk '{print $4}' | sed 's/+//'| sed 's/.0°C//')
temp=${temp%???}

if [ "$temp" -ge "$temp2" ] ; then
    echo "Cpu: <fc=#C1514E>$temp</fc>°C"
elif [ "$temp" -ge "$temp1" ] ; then
    echo "Cpu: <fc=#C1A24E>$temp</fc>°C"
else
    echo "Cpu: <fc=#AAC0F0>$temp</fc>°C"

fi
