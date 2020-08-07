#!/bin/sh

export LANG=ru_RU.UTF-8

# XDG
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache
export XDG_CONFIG_HOME=$HOME/.config

# ~/.Xauthority
#export XAUTHORITY=$XDG_CACHE_HOME/Xauthority

# Go
export GOPATH_BASE=$XDG_DATA_HOME/go
export GOPATH=$GOPATH_BASE:$HOME/projects/home/go:$HOME/projects/work/go:$HOME/projects/test/go

# Rust
export CARGO_HOME=$XDG_DATA_HOME/cargo

# ZSH
export ZDOTDIR=$XDG_CONFIG_HOME/zsh

# Mplayer
export MPLAYER_HOME=$XDG_CONFIG_HOME/mplayer

# Python
export PYTHONSTARTUP=$XDG_CONFIG_HOME/python/pythonrc
export PYTHON_EGG_CACHE=$XDG_CACHE_HOME/python-eggs

# Gimp
export GIMP2_DIRECTORY=$XDG_DATA_HOME/gimp-2.8

# KDE
export KDEHOME=$XDG_DATA_HOME/kde

# gnupg
export GNUPGHOME=$XDG_CONFIG_HOME/gnupg

# java font
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

# Other apps
export __GL_SHADER_DISK_CACHE_PATH=$XDG_CACHE_HOME/nv
export CUDA_CACHE_PATH=$XDG_CACHE_HOME/nv
export LESSHISTFILE=$XDG_CACHE_HOME/.lesshst

# Aliases
export SHELL=`which zsh`
SUBL_PATH=$(which subl3) && export EDITOR=$SUBL_PATH

# Lib paths
export LD_LIBRARY_PATH=$XDG_CONFIG_HOME/lib:$LD_LIBRARY_PATH

# Bin paths
export PATH=$XDG_CONFIG_HOME/bin:$PATH:$GOPATH_BASE/bin


echo $(date) >> ~/date
