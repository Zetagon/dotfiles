#!/usr/bin/env bash


#Reset
pkill xcape
setxkbmap -option
setxkbmap us

setxkbmap -option "caps:ctrl_modifier"
setxkbmap -layout us,se -option grp:shifts_toggle

# Make tab a superkey when held down, tab when pressed alone
xmodmap -e "keysym Tab = Hyper_L"
xmodmap -e "remove mod4 = Hyper_L"
xmodmap -e "keycode any = Tab"
xcape -e "Hyper_L=Tab"
xcape -e "Caps_Lock=Escape"
