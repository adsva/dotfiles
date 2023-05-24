#!/bin/bash
dbus-monitor --session "type='signal',interface='org.gnome.ScreenSaver'" | \
(
  while read X; do
    if [[ "$X" =~ true ]]; then
     echo "Screen locked, pausing audio" 
     playerctl pause;
    elif [[ "$X" =~ false ]]; then
     echo "Screen unlocked" 
    fi
  done
)
