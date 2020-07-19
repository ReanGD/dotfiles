autoload -U colors && colors

setopt PROMPT_SUBST

_print_reset_color() {
  echo -n "%{$reset_color%}$1"
}

_print_with_color() {
  echo -n "%{$fg[$1]%}$2"
}

_print_with_bold_color() {
  echo -n "%{$terminfo[bold]$fg[$1]%}$2%{$reset_color%}"
}

_prompt_start() {
    _print_with_bold_color blue "# "
    [[ $RETVAL -ne 0 ]] && _print_with_bold_color red "✘($RETVAL) "
}

_prompt_context() {
  if [[ "$USER" != "rean" || -n "$SSH_CLIENT" ]]; then
    _print_with_color red "%n@%m: "
  fi
}

_prompt_dir() {
    _print_with_bold_color yellow "${PWD/#$HOME/~} "
}

_prompt_git() {
    if [ "$(_git_command rev-parse --is-inside-work-tree 2> /dev/null)" ]; then
      _print_with_color white "on "
      _print_reset_color "git:"

      # parse git user name
      if [[ "$(_git_command config user.name)" == "ReanGD" ]]; then
          _print_with_color green "home "
      else
          _print_with_color magenta "work "
      fi

      # parse git current branch
      local ref
      ref=$(_git_command symbolic-ref --quiet HEAD 2> /dev/null)
      local ret=$?
      if [[ $ret != 0 ]]; then
        [[ $ret == 128 ]] && return  # no git repo.
        ref=$(_git_command rev-parse --short HEAD 2> /dev/null) || return
      fi
      _print_with_color cyan "${ref#refs/heads/} "

      # parse git dirty
      if [[ -n $(_git_command status --porcelain 2> /dev/null | tail -n1) ]]; then
        _print_with_color red "x "
      else
        _print_with_color green "o "
      fi
  fi
}

_prompt_end() {
    echo ""
    _print_with_bold_color red "$ "
}

_prompt_build() {
    RETVAL=$?
    _prompt_start
    _prompt_context
    _prompt_dir
    _prompt_git
    _prompt_end
}

PROMPT='
$(_prompt_build)'
