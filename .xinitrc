#!bin/sh

# APPS
export ZDOTDIR=$HOME/.config/zsh
export MPLAYER_HOME=$HOME/.config/mplayer
export CARGO_HOME=$HOME/.local/share/cargo
export GOPATH=$HOME/.local/share/go
export GNUPGHOME=$HOME/.config/gnupg
export GIMP2_DIRECTORY=$HOME/.local/share/gimp-2.8

# LOCAL PATHS
export PATH=$HOME/.config/bin:$PATH:$GOPATH/bin

# AUTOLOAD
xrdb ~/.config/.Xresources
eval $(ssh-agent)

/usr/bin/xmodmap ~/.Xmodmap &
setxkbmap -option 'ctrl:nocaps' &
# urxvt &
# subl3 &
# doublecmd &
firefox &
kbdd &
start-pulseaudio-x11 &
volumeicon &
ssh-add ~/.ssh/github &
ssh-add ~/.ssh/gitlab &
awesome
