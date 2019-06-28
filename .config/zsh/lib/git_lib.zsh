function git_user_localtion() {
    if [[ "$(command git config user.name)" == "ReanGD" ]]; then
        echo "$ZSH_THEME_GIT_PROMPT_HOME"
    else
        echo "$ZSH_THEME_GIT_PROMPT_WORK"
    fi
}

function git_prompt_info_with_location() {
  local ref
  if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" != "1" ]]; then
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0

    echo "$ZSH_THEME_GIT_PROMPT_PREFIX$(git_user_localtion)$ZSH_THEME_GIT_PROMPT_BRANCH_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

function git_change_user() {
	if [[ "$(command git config user.name)" == "ReanGD" ]]; then
        cp $XDG_CONFIG_HOME/git/config.work $XDG_CONFIG_HOME/git/config
    else
        cp $XDG_CONFIG_HOME/git/config.home $XDG_CONFIG_HOME/git/config
    fi
}
