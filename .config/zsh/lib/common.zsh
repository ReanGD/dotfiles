_git_command() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}

_git_current_branch() {
  local ref
  ref=$(_git_command symbolic-ref --quiet HEAD 2> /dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return  # no git repo.
    ref=$(_git_command rev-parse --short HEAD 2> /dev/null) || return
  fi
  echo ${ref#refs/heads/}
}

# fix backspace in ssh
TERM=xterm-256color

PAGER='less'
LESS='-R'

FZF_COLOR="--color=fg:-1,bg:-1,hl:33,fg+:254,bg+:235,hl+:33,info:136,prompt:136,pointer:230,marker:230,spinner:136"
