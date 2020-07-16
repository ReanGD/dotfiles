# my options
unsetopt beep
setopt IGNORE_EOF

mkdir -p ~/.local/share/zsh

# alias
alias st="subl3"
alias ping='ping -c 4'
alias dcc='docker-compose'

# Path to oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh

# Path to .zcompdump<...> file
ZSH_COMPDUMP="${XDG_CACHE_HOME}/zsh/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="simple"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

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

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=~/.config/zsh

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
export ZSH_COLORIZE_TOOL="pygmentize"
plugins=(z git command-not-found colored-man-pages colorize compleat docker extract)

source $ZSH/oh-my-zsh.sh

# User configuration
source $ZSH_CUSTOM/lib/settings.zsh
