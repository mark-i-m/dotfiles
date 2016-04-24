# Flashing battery status script coded to be used with Xmobar
#
# Usage Example:
#   > get_battery_status
#   BAT: <fc=#005500>100%</fc>
#
# Azer Koculu <azer@kodfabrik.com>
# http://azer.kodfabrik.com

flash=$(date +%s)

charging=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep state | grep -q dis; echo $?)
percent=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep percentage | grep -o "[0-9]*")

num_bars=5
low_pct=$((100 / num_bars))

color_normal="grey"
color_low="red"

label="Bat:"
pounds=$(head -c $((percent * num_bars / 100)) < /dev/zero | tr '\0' '#')
dashes=$(head -c $((num_bars - percent * num_bars / 100)) < /dev/zero | tr '\0' '-')
color=$color_normal

if [ $percent -lt $low_pct ]; then
    color=$color_low
fi

if [ $percent -lt $low_pct ] && [ $charging -eq 0 ]; then
    python /home/mark/.xmonad/low_bat.py $percent &
fi

if [ $((flash % 2)) -eq 0 ] && [ $percent -lt $low_pct ] && [ $charging -eq 0 ]; then
    color="yellow"
fi

echo "$label <fc=$color>[$pounds$dashes] $percent%</fc>"
