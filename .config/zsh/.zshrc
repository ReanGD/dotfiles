ZSH_DATA_DIR=$XDG_DATA_HOME/zsh
ZSH_CACHE_DIR=$XDG_CACHE_HOME/zsh
ZSH_CONFIG_DIR=$XDG_CONFIG_HOME/zsh
ZSH_LIB_DIR=$XDG_CONFIG_HOME/zsh/lib
ZSH_COMPDUMP="${ZSH_DATA_DIR}/.zcompdump-${HOST/.*/}-${ZSH_VERSION}"

declare -A ZINIT
ZINIT[ZCOMPDUMP_PATH]=ZSH_COMPDUMP

# Install zinit
if [[ ! -f $ZSH_CONFIG_DIR/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$ZSH_CONFIG_DIR/.zinit" && command chmod g-rwX "$ZSH_CONFIG_DIR/.zinit"
    command git clone https://github.com/zdharma/zinit "$ZSH_CONFIG_DIR/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$ZSH_CONFIG_DIR/.zinit/bin/zinit.zsh"

source $ZSH_LIB_DIR/common.zsh
source $ZSH_LIB_DIR/options.zsh
source $ZSH_LIB_DIR/history.zsh
source $ZSH_LIB_DIR/aliases.zsh
source $ZSH_LIB_DIR/correction.zsh
source $ZSH_LIB_DIR/prompt.zsh
source $ZSH_LIB_DIR/key_bindings.zsh
source $ZSH_LIB_DIR/completion.zsh

# zinit snippet OMZP::docker
# zinit snippet OMZP::compleat
# zinit load mbrubeck/compleat
zinit snippet OMZP::extract
zinit ice atinit'ZSH_COLORIZE_STYLE="monokai"'
zinit snippet OMZP::colorize
zinit load agkozak/zsh-z
zinit load supercrabtree/k
zinit load zpm-zsh/clipboard
zinit load trystan2k/zsh-tab-title
zinit load zsh-users/zsh-autosuggestions
zinit ice atinit'ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10"'
zinit load zdharma/fast-syntax-highlighting

source $ZSH_LIB_DIR/compinit.zsh
