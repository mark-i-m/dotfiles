#!/bin/bash

v1=`amixer get Master | grep -o -G "\[[0-9]*%\]"`
v2=`amixer get Master | fgrep -o [on]`
v3=`amixer get Master | fgrep -o [off]`

#output string
v4=""

if [ "$v2" = "[on]" ]; then
    v4="$v1"
fi

if [ "$v3" = "[off]" ]; then
    v4="$v1[mute]"
fi

echo "Vol: $v4"
