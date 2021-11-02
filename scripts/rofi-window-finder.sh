#!/bin/bash

list=$(wmctrl -l | cut -c 12-)
windows=""
while read -r line
do
  num=${line:0:1}
  let num+=1
  [[ $num -gt 9 ]] && continue

  win=$(echo ${line} | awk '{$1=$2=$(NF-1)=$NF=""; print $0}')
  if [[ ${#win} -gt 2 ]]
  then
    win="- "$win
  fi
  title=$(echo ${line} | awk '{print $NF}')
  win="$num     $title   ${win}"
  windows+="$win\n"
done <<< "$list"

result=$(printf "$windows" | rofi -dmenu)
result=${result:0:1}
let result-=1
wmctrl -s $result
