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
  _print_with_bold_color yellow "%~ "
}

_git_dirty() {
  _git_command diff --quiet 2>/dev/null && \
  _git_command diff --cached --quiet 2>/dev/null
}

_prompt_git() {
  local branch
  branch="$(_git_current_branch)" || return 0
  [[ -z "$branch" ]] && return 0

  local user_name
  user_name="$(_git_command config user.name 2>/dev/null)"

  local label label_color
  case "$user_name" in
    ReanGD) label="home";    label_color="green"   ;;
    *)      label="work";    label_color="magenta" ;;
  esac

  _print_with_color white "on "
  _print_reset_color "git:"
  _print_with_color "$label_color" "$label "
  _print_with_color cyan "$branch "

  if _git_dirty; then
    _print_with_color green "✓ "
  else
    _print_with_color red "✗ "
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
