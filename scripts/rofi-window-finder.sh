#!/bin/bash

list=$(wmctrl -x -l | cut -c 12-)
windows=""

ln=0
while read -r line
do
  class=$(echo "$line" | awk '{print $2}' | cut -f2 -d".")
  class=${#class}
  if [[ $class -gt $ln ]]
  then
    ln=$class
  fi
done <<< "$list"

while read -r line
do
  workspace=$(echo "$line" | awk '{print $1}')
  [[ $workspace -eq 18 ]] && continue
  let workspace+=1
  if [[ $workspace -lt 10 ]]; then
    screen="1:"
  else
    screen="2:"
    let workspace-=9
  fi

  class=$(echo "$line" | awk '{print $2}' | cut -f2 -d".")
  class_sep=""
  if [[ ${#class} -lt $ln ]]
  then
   diff=$((ln - ${#class}))
   class_sep=$(printf "%-${diff}s" $MESSAGE)
  fi

  name=$(echo "$line" | awk '{$1=$2=$3=""; print $0}')

  windows+="$screen $workspace      $class$class_sep   $name\n"
done <<< "$list"

i=0
workspaces=$(wmctrl -d)
while read -r line
do
  tmp=$(echo $line | awk '{print $2}')
  [[ "$tmp" == "development" ]] && break
  let i+=1
done <<< "$workspaces"

result=$(printf "$windows" | rofi -dmenu)
screen=${result:0:1}
workspace=${result:3:1}
if [[ $screen == "2" ]]
then
  let workspace+=8
  [[ $i -lt 9 ]] && xdotool key super+o
else
  let workspace-=1
  [[ $i -gt 8 ]] && xdotool key super+o
fi
wmctrl -s $workspace
