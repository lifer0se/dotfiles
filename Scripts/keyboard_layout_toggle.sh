#!/bin/sh

LANG=`setxkbmap -query | awk '/layout/{print $2}'`

LANGSET=false
for i in $(seq 1 1 $#)
do
   TARGET="${!i}"
   if [ "$LANGSET" = true ]
   then
       break
   elif [ $LANG = $TARGET ]
   then
       LANGSET=true
   fi
done

if [ $LANG = $TARGET ]
then
    TARGET=$1
fi

setxkbmap -layout $TARGET
