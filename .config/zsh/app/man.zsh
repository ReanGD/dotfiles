# Color man pages through less; use bold instead of blinking.
man() {
  local reset="$(tput sgr0)"
  local bold="$(tput bold)$(tput setaf 4)"
  env \
    LESS_TERMCAP_md="$bold" \
    LESS_TERMCAP_me="$reset" \
    LESS_TERMCAP_mb="$bold" \
    LESS_TERMCAP_us="$(tput setaf 2)" \
    LESS_TERMCAP_ue="$reset" \
    LESS_TERMCAP_so="$(tput smso)" \
    LESS_TERMCAP_se="$(tput rmso)" \
    GROFF_NO_SGR=1 \
    MANPAGER='less -Q' \
    man "$@"
}

# Search manual descriptions and open the selected name and section.
fman() {
  local selection
  local pattern='^([^[:space:],]+)[^(]*\(([^()]+)\)'
  local -a match mbegin mend
  selection=$(command man -k . | fzf --prompt='Man> ') || return
  [[ -n "$selection" ]] || return 0
  [[ "$selection" =~ "$pattern" ]] || return 1
  man -- "$match[2]" "$match[1]"
}