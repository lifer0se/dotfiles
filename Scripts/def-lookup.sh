#!/bin/bash

word=$(xclip -o)
res=$(curl -s "https://api.dictionaryapi.dev/api/v2/entries/en_US/$word")
regex=$'"definition":"\K(.*?)(?=")'
definitions=$(echo $res | grep -Po "$regex")
separated=$(sed ':a;N;$!ba;s/\n/\n\n/g' <<< "$definitions")
notify-send -u low -t 10000 "$word" "$separated"
