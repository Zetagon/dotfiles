#!/bin/bash
# setxkbmap -option "caps:ctrl_modifier"
# setxkbmap -layout us,se -option grp:rctrl_toggle
# setxkbmap -layout us,se -option grp:shifts_toggle 

xrdb -merge .Xresources
setxkbmap -option "caps:ctrl_modifier"
setxkbmap -layout us,se -option grp:shifts_toggle 

emacs --daemon
firefox &
slack &
spotify &
dropbox start
guake 

    # Log stderror to a file 
exec awesome 
    # No error logging
    #dwm >/dev/null 2>&1

