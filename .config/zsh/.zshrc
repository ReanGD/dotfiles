# ZDOTDIR is set by the session environment; no ~/.zshenv bootstrap is needed.
ZSH_CONFIG_DIR="${ZDOTDIR:-${XDG_CONFIG_HOME:-$HOME/.config}/zsh}"
ZSH_DATA_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh"
ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
ZSH_LIB_DIR="$ZSH_CONFIG_DIR/lib"
ZSH_APP_DIR="$ZSH_CONFIG_DIR/app"
ZSH_COMPDUMP="$ZSH_CACHE_DIR/.zcompdump-$HOST-$ZSH_VERSION"

command mkdir -p -- "$ZSH_DATA_DIR" "$ZSH_CACHE_DIR" || return

source "$ZSH_LIB_DIR/common.zsh"
source "$ZSH_LIB_DIR/options.zsh"
source "$ZSH_APP_DIR/main.zsh"
source "$ZSH_LIB_DIR/correction.zsh"
source "$ZSH_LIB_DIR/prompt.zsh"
source "$ZSH_LIB_DIR/key_bindings.zsh"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=10'
export ANTIDOTE_HOME="$ZSH_DATA_DIR/plugins"
zstyle ':antidote:bundle' file "$ZSH_CONFIG_DIR/plugins.txt"
zstyle ':antidote:static' file "$ZSH_CACHE_DIR/plugins.zsh"

# Bootstrap once. Subsequent starts use local files, without checking for updates.
# Update the manager and plugins explicitly with: zsh_update
() {
    local antidote_dir="$ZSH_DATA_DIR/antidote"
    if [[ ! -r "$antidote_dir/antidote.zsh" ]]; then
        command git clone --depth=1 https://github.com/mattmc3/antidote.git "$antidote_dir" || return
    fi
    source "$antidote_dir/antidote.zsh" || return
    antidote load
} || return

# Plugin completions must be on fpath before compinit; app/ queues compdef calls.
source "$ZSH_LIB_DIR/compinit.zsh"
