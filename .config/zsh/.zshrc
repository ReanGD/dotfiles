ZSH_DATA_DIR=$XDG_DATA_HOME/zsh
ZSH_CACHE_DIR=$XDG_CACHE_HOME/zsh
ZSH_CONFIG_DIR=$XDG_CONFIG_HOME/zsh
ZSH_LIB_DIR=$XDG_CONFIG_HOME/zsh/lib

source $ZSH_LIB_DIR/common.zsh
source $ZSH_LIB_DIR/options.zsh
source $ZSH_LIB_DIR/history.zsh
source $ZSH_LIB_DIR/aliases.zsh
source $ZSH_LIB_DIR/correction.zsh
source $ZSH_LIB_DIR/prompt.zsh
source $ZSH_LIB_DIR/key_bindings.zsh
source $ZSH_LIB_DIR/completion.zsh


# my options
unsetopt beep
setopt IGNORE_EOF

# alias
alias st="subl3"
alias ping='ping -c 4'
alias dcc='docker-compose'

# Path to oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh

# Path to .zcompdump<...> file
ZSH_COMPDUMP="${XDG_CACHE_HOME}/zsh/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=~/.config/zsh

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
export ZSH_COLORIZE_TOOL="pygmentize"
plugins=()

source $ZSH_CUSTOM/lib/oh-my-zsh-local.sh

# Local modules
source $ZSH_CUSTOM/lib/settings.zsh
source $ZSH_CUSTOM/lib/key-bindings.zsh
source $ZSH_CUSTOM/themes/simple.zsh-theme

# Install zinit
if [[ ! -f $HOME/.config/zsh/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.config/zsh/.zinit" && command chmod g-rwX "$HOME/.config/zsh/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.config/zsh/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.config/zsh/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=10'

zinit snippet OMZP::git
# zinit snippet OMZP::docker
# zinit snippet OMZP::compleat
zinit snippet OMZP::extract
zinit snippet OMZP::colorize
zinit snippet OMZP::command-not-found
zinit snippet OMZP::colored-man-pages
zinit load agkozak/zsh-z
zinit load supercrabtree/k
zinit load zsh-users/zsh-syntax-highlighting
zinit load zsh-users/zsh-autosuggestions
