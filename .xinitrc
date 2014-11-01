#!bin/sh

# LOCAL PATHS
export PATH=$HOME/.config/bin:$PATH

# APPS
export MPLAYER_HOME=$HOME/.config/mplayer
export PENTADACTYL_INIT=":source $HOME/.config/firefox/.pentadactylrc"

# AUTOLOAD
xrdb ~/.Xresources
eval $(ssh-agent)

# /usr/bin/xmodmap ~/.Xmodmap &
# setxkbmap -option 'ctrl:nocaps' &
urxvt &
subl3 &
firefox &
doublecmd &
kbdd &
start-pulseaudio-x11 &
ssh-add ~/.ssh/github &
qtile
