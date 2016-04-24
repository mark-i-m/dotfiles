#!/bin/bash
mem=$(free -m | awk 'NR==2{printf "%.0f%%", $3*100/$2 }')
echo "Mem: [$mem]"
