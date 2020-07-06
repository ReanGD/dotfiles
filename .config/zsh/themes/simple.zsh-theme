print_reset_color() {
  echo -n "%{$reset_color%}$1"
}

print_with_color() {
  echo -n "%{$fg[$1]%}$2"
}

print_with_bold_color() {
  echo -n "%{$terminfo[bold]$fg[$1]%}$2%{$reset_color%}"
}

prompt_start() {
    print_with_bold_color blue "# "
    [[ $RETVAL -ne 0 ]] && print_with_bold_color red "✘($RETVAL) "
}

prompt_context() {
  if [[ "$USER" != "rean" || -n "$SSH_CLIENT" ]]; then
    print_with_color red "%n@%m: "
  fi
}

prompt_dir() {
    print_with_bold_color yellow "${PWD/#$HOME/~} "
}

ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}x"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg[green]%}o"

prompt_git() {
    if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" != "1" ]]; then
        local ref
        ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
        ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0

        print_with_color white "on "
        print_reset_color "git:"
        if [[ "$(command git config user.name)" == "ReanGD" ]]; then
            print_with_color green "home "
        else
            print_with_color magenta "work "
        fi
        print_with_color cyan "${ref#refs/heads/}$(parse_git_dirty) "
    fi
}

prompt_end() {
    echo ""
    print_with_bold_color red "$ "
}

build_prompt() {
    RETVAL=$?
    prompt_start
    prompt_context
    prompt_dir
    prompt_git
    prompt_end
}

PROMPT='
$(build_prompt)'
