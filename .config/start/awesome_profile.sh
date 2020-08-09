#!/bin/sh
# start before awesome

xrdb -merge ~/.config/start/xresources


# caps lock as layout switch v1
# see: https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Using_setxkbmap
setxkbmap -option grp:caps_toggle

# caps lock as layout switch v2
# see
# xmodmap -pke | grep 66
# less /usr/include/X11/XF86keysym.h
#
# xmodmap -e 'keycode 66 = ISO_Next_Group ISO_Next_Group ISO_Next_Group ISO_Next_Group'
