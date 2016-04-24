#!/bin/bash

pkill trayer
pkill nm-applet
xmonad --recompile && xmonad --restart
