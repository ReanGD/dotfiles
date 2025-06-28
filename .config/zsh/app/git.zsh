_git_command() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}

_git_current_branch() {
  local ref
  ref=$(_git_command symbolic-ref --quiet HEAD 2>/dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return  # no git repo
    ref=$(_git_command rev-parse --short HEAD 2>/dev/null) || return
  fi
  print -r -- "${ref#refs/heads/}"
}

_ggpull() {
  local branch
  branch=$(_git_current_branch) || { echo "Not a git repo"; return 1; }
  git pull origin "$branch"
}

ggpush() {
  local branch
  branch=$(_git_current_branch) || { echo "Not a git repo"; return 1; }
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
