export ATUIN_HOME="${XDG_STATE_HOME:-$HOME/.local/state}/atuin"
export ATUIN_CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/atuin"

if [[ $options[zle] = on ]]; then
    eval "$(ATUIN_NOBIND=true atuin init zsh)"

    _atuin_search_dir() {
        _atuin_search --filter-mode directory
    }
    zle -N atuin-search-directory _atuin_search_dir
fi
