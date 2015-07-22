#!bin/sh

# LOCAL PATHS
export PATH=$HOME/.config/bin:$PATH

# APPS
export ZDOTDIR=$HOME/.config/zsh
export MPLAYER_HOME=$HOME/.config/mplayer
export CARGO_HOME=$HOME/.local/share/cargo

# AUTOLOAD
xrdb ~/.config/.Xresources
eval $(ssh-agent)

/usr/bin/xmodmap ~/.Xmodmap &
setxkbmap -option 'ctrl:nocaps' &
# urxvt &
# subl3 &
# firefox &
# doublecmd &
kbdd &
start-pulseaudio-x11 &
ssh-add ~/.ssh/github &
ssh-add ~/.ssh/gitlab &
awesome
