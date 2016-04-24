#!/bin/bash

v1=`amixer get Master | grep -o -G "\[[0-9]*%\]"`
v2=`amixer get Master | fgrep -o [on]`
v3=`amixer get Master | fgrep -o [off]`

if [ "$v2" = "[on]" ]; then
    amixer -D pulse sset Master 1+ toggle
fi

if [ "$v3" = "[off]" ]; then
    amixer -D pulse sset Master 1+ toggle
    #amixer set Speaker 1+ toggle
    #amixer set Headphone 1+ toggle
    #amixer set PCM 1+ toggle
fi
