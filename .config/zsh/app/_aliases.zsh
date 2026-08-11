# Trailing spaces enable alias expansion for the following word.
alias -- sudo='sudo '
alias -- sudoe='sudo --preserve-env '

alias st='subl'
alias ping='ping -c 4'
alias pcurl='_with_proxy curl'
alias pwget='_with_proxy wget'

alias umount='sudo umount'
alias mount='sudo mount'
alias df='df --human-readable --print-type'
alias du='du --human-readable --total'

# Disable audible and visual bells in less.
alias less='less -Q'

# Strip ANSI SGR formatting, including colored zap log levels.
alias uncolor='sed -E "s/\x1B\[[0-9;:]*m//g"'

alias diff='diff --color=auto'
alias grep='grep --color=auto --exclude-dir=.git'
