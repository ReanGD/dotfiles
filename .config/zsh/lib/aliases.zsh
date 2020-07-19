alias st="subl3"
alias subl="subl3"
alias ping='ping -c 4'

# Find the option for using colors in ls, depending on the version
# For GNU ls, we use the default ls color theme. They can later be overwritten by themes.
if [[ -z "$LS_COLORS" ]]; then
  (( $+commands[dircolors] )) && eval "$(dircolors -b)"
fi

alias ls='ls --color=tty'

# Take advantage of $LS_COLORS for completion as well.
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

alias diff='diff --color'

# Ignore these folders in grep
EXC_FOLDERS="{.bzr,CVS,.git,.hg,.svn,.idea,.tox}"

alias grep="grep --color=auto --exclude-dir=$EXC_FOLDERS"
alias egrep="egrep --color=auto --exclude-dir=$EXC_FOLDERS"
alias fgrep="fgrep --color=auto --exclude-dir=$EXC_FOLDERS"
