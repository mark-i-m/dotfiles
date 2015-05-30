xsetroot -cursor_name left_ptr

# xsetroot -solid "#555555"
xsetroot -solid "#000000"

xautolock -time 5 -locker "slock" &

wmname xmonad

# systray
. ~/.xmonad/systray.sh &

# start xmonad
exec /usr/bin/xmonad
