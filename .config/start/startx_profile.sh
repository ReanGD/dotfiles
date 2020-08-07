#!bin/sh

DE=$1

xrdb -merge ~/.config/start/xresources

if [ "$DE" = "awesome" ]; then
# see
# xmodmap -pke | grep 66
# less /usr/include/X11/XF86keysym.h

# caps lock as layout switch
xmodmap -e 'keycode 66 = ISO_Next_Group ISO_Next_Group ISO_Next_Group ISO_Next_Group'

fi
