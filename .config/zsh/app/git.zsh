_git_command() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}

_git_current_branch() {
  local ref
  ref=$(_git_command symbolic-ref --quiet HEAD 2>/dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return "$ret"  # no git repo
    ref=$(_git_command rev-parse --short HEAD 2>/dev/null) || return
  fi
  print -r -- "${ref#refs/heads/}"
}

_git_branch_for_sync() {
  local ref
  ref=$(_git_command symbolic-ref --quiet HEAD 2>/dev/null)
  local ret=$?
  if (( ret == 1 )); then
    print -u2 -- 'No branch checked out. Switch to a branch first.'
    return 1
  elif (( ret != 0 )); then
    print -u2 -- 'Not a git repo'
    return 1
  fi
  print -r -- "${ref#refs/heads/}"
}

_ggpull() {
  local branch
  branch=$(_git_branch_for_sync) || return
  git pull origin "$branch"
}

_ggpush() {
  local branch
  branch=$(_git_branch_for_sync) || return
  git push origin "$branch"
}

alias g='git'
alias ga='git add'
alias gci='git commit'
alias gco='git checkout'
alias gst='git status'
alias gfa='git fetch --all --prune'
alias ggpull='_ggpull'
alias ggpush='_ggpush'
