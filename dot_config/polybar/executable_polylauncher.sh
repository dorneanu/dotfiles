#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done


# Get monitors
MONITORS=($(xrandr --listactivemonitors |  tail -n +2 | grep -oE '[^ ]+$'))
MONITORLENGTH=${#MONITORS[@]}


for (( i=0; i<${MONITORLENGTH}; i++ ));
do
  if [ "$i" -eq 0 ]; then
    echo "launching tray on ${MONITORS[$i]}"
    TRAY_POS=right MONITOR=${MONITORS[$i]} polybar example &
  else
    MONITOR=${MONITORS[$i]} polybar example &
  fi
done

