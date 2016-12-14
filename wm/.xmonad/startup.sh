#!/bin/bash

# start systray
/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --transparent true --alpha 0 --tint 0x000000 --width 2 --height 18 &
/usr/bin/nm-applet &

# start automatic screen locking
xautolock -time 10 -locker "slock" &

# set window manager name
wmname xmonad

# set trackpad properties
xinput set-prop "AlpsPS/2 ALPS GlidePoint" "Synaptics Two-Finger Scrolling" 1 1
xinput set-prop "AlpsPS/2 ALPS GlidePoint" "Synaptics Palm Detection" 1
xinput set-prop "AlpsPS/2 ALPS GlidePoint" "Synaptics Tap Action" 0 0 0
xinput set-prop "AlpsPS/2 ALPS GlidePoint" "Synaptics Edge Scrolling" 0 0 0
xinput set-prop "AlpsPS/2 ALPS GlidePoint" "Synaptics Finger" 10 30 0
xinput set-prop "AlpsPS/2 ALPS GlidePoint" "Synaptics Noise Cancellation" 10 10
