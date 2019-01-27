#!/bin/bash
# setxkbmap -option "caps:ctrl_modifier"
# setxkbmap -layout us,se -option grp:rctrl_toggle
# setxkbmap -layout us,se -option grp:shifts_toggle 

#xrdb -merge .Xresources
#setxkbmap -option "caps:ctrl_modifier"
#setxkbmap -layout us,se -option grp:shifts_toggle 
## setxkbmap -layout dvorak,se -option grp:shifts_toggle -option lv3:ralt_switch
##setxkbmap -layout se,dvorak -variant dvorak, -option grp:shifts_toggle -option lv3:ralt_switch

## setxkbmap -layout dvorak,se  -option grp:shifts_toggle -option lv3:ralt_switch
#amixer -D pulse set Master 50%


#emacs --daemon
## firefox &
#qutebrowser &
#slack &
#spotify &
#dropbox start
#guake  &

xbindkeys
#    # Log stderror to a file 
#exec awesome 
#    # No error logging
#    #dwm >/dev/null 2>&1
setxkbmap -option "caps:ctrl_modifier"
setxkbmap -layout us,se -option grp:shifts_toggle

# Make tab a superkey when held down, tab when pressed alone
xmodmap -e "keysym Tab = Hyper_L"
xmodmap -e "remove mod4 = Hyper_L"
xmodmap -e "keycode any = Tab"
xcape -e "Hyper_L=Tab"

node ~/bin/nasaApodImage.js
feh --bg-max ~/Pictures/background.jpg
deja-dup &
dropbox start &
emacs --daemon
xmonad
