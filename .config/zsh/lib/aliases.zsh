alias st="subl3"
alias subl="subl3"
alias ping='ping -c 4'

# Find the option for using colors in ls, depending on the version
# For GNU ls, we use the default ls color theme. They can later be overwritten by themes.
if [[ -z "$LS_COLORS" ]]; then
  (( $+commands[dircolors] )) && eval "$(dircolors -b)"
fi

ls --color -d . &>/dev/null && alias ls='ls --color=tty' || { ls -G . &>/dev/null && alias ls='ls -G' }

# Take advantage of $LS_COLORS for completion as well.
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Enable diff color if possible.
if command diff --color . . &>/dev/null; then
  alias diff='diff --color'
fi
