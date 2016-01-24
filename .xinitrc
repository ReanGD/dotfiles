#!bin/sh

# APPS
export ZDOTDIR=$HOME/.config/zsh
export MPLAYER_HOME=$HOME/.config/mplayer
export CARGO_HOME=$HOME/.local/share/cargo
export GOPATH_BASE=$HOME/.local/share/go
export GOPATH=$GOPATH_BASE:$HOME/projects/git/go:$HOME/projects/temp/go
export GNUPGHOME=$HOME/.config/gnupg
export PYTHONSTARTUP=/home/rean/.config/python/pythonrc
export GIMP2_DIRECTORY=$HOME/.local/share/gimp-2.8
export __GL_SHADER_DISK_CACHE_PATH=$HOME/.cache/nv
export CUDA_CACHE_PATH=$HOME/.cache/nv
export KDEHOME=$HOME/.local/share/kde
export LESSHISTFILE=$HOME/.cache/.lesshst
export PYTHON_EGG_CACHE=$HOME/.cache/python-eggs

# LOCAL PATHS
export PATH=$HOME/.config/bin:$PATH:$GOPATH_BASE/bin

# AUTOLOAD
autoload () {
	/usr/bin/xmodmap ~/.Xmodmap &
	setxkbmap -option 'ctrl:nocaps' &
	kbdd
	# urxvt &
	(sleep 5 && firefox & subl3 & doublecmd) &
	(sleep 10 && volumeicon) &
	(sleep 15 && ssh-add ~/.ssh/github & ssh-add ~/.ssh/gitlab) &
	(sleep 25 && ~/.config/bin/dropbox)
}

# START
xrdb ~/.config/.Xresources
eval $(ssh-agent)
autoload & awesome
