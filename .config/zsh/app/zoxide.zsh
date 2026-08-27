# Initialize after compinit; keep cd and existing key bindings unchanged.
# z <query><Tab>: complete from the database without executing the jump.
# zi keeps explicit fzf selection; cd and existing key bindings stay unchanged.
_zoxide_history_complete() {
  (( CURRENT == 2 )) || return 1
  local -a matches
  matches=("${(@f)$(command zoxide query --list --exclude "$PWD" -- "${PREFIX}${SUFFIX}")}")
  matches=("${(@)matches:#}")
  (( ${#matches} )) || return 1
  compadd -U -V zoxide-history -a matches
  compstate[insert]=menu
  compstate[list]=list
}

_post_compinit+=('eval "$(zoxide init zsh --hook pwd)"' 'compdef _zoxide_history_complete z' 'compdef _directories zi')
