#!/bin/bash

killall trayer nm-applet;
xmonad --recompile && xmonad --restart

if ! [[ "$(pidof trayer)" && "$(pidof nm-applet)" ]]; then
    . ~/.xmonad/systray.sh
fi
