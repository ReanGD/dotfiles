#!bin/sh

xrdb -merge ~/.config/start/xresources
# disable SHIFT+CAPS LOCK
xmodmap -e 'keycode 66 = ISO_Next_Group ISO_Next_Group ISO_Next_Group ISO_Next_Group'

echo $(date) >> ~/xdate
