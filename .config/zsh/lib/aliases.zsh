alias st="subl3"
alias subl="subl3"
alias ping='ping -c 4'

# Find the option for using colors in ls, depending on the version
# For GNU ls, we use the default ls color theme. They can later be overwritten by themes.
if [[ -z "$LS_COLORS" ]]; then
  (( $+commands[dircolors] )) && eval "$(dircolors -b)"
fi

if which exa >/dev/null; then
    alias ls='exa --grid'
    alias ll='exa --long --group --header --links --all --git'
else
    alias ls='ls --color=tty'
    alias ll='ls -la'
fi

# Take advantage of $LS_COLORS for completion as well.
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

alias diff='diff --color'

# Ignore these folders in grep
EXC_FOLDERS="{.bzr,CVS,.git,.hg,.svn,.idea,.tox}"

alias grep="grep --color=auto --exclude-dir=$EXC_FOLDERS"
alias egrep="egrep --color=auto --exclude-dir=$EXC_FOLDERS"
alias fgrep="fgrep --color=auto --exclude-dir=$EXC_FOLDERS"

_show_which() {
  OUTPUT=$(which $1 | cut -d " " -f7-)
  echo "Running '$OUTPUT'" 1>&2
}

alias umount='_show_which umount && sudo umount'
alias mount='_show_which mount && sudo mount'
alias df='_show_which df && df -k --print-type --human-readable'
alias du='_show_which du && du -k --total --human-readable'
alias wifi-menu='_show_which wifi-menu && sudo wifi-menu'
alias journalctl='_show_which journalctl && sudo journalctl'
alias systemctl='_show_which systemctl && sudo systemctl'

# disable beep in less
alias less='less -Q'

# git aliases
alias g='git'
alias ga='git add'
alias gci='git commit'
alias gco='git checkout'
alias gst='git status'
alias gfa='git fetch --all --prune'
alias ggpull='git pull origin "$(_git_current_branch)"'
alias ggpush='git push origin "$(_git_current_branch)"'

# auto add sudo and rehash
pacman() {
  /usr/bin/sudo /usr/bin/pacman $* && echo "$*" | grep -q "S\|R" && rehash
}

# auto rehash
yay() {
  /usr/bin/yay $* && echo "$*" | grep -q "S\|R" && rehash
}

# Select and kill process - list only the ones you can kill
fkill() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

# Colorized man
man() {
	env \
		LESS_TERMCAP_md=$(tput bold; tput setaf 4) \
		LESS_TERMCAP_me=$(tput sgr0) \
		LESS_TERMCAP_mb=$(tput blink) \
		LESS_TERMCAP_us=$(tput setaf 2) \
		LESS_TERMCAP_ue=$(tput sgr0) \
		LESS_TERMCAP_so=$(tput smso) \
		LESS_TERMCAP_se=$(tput rmso) \
		PAGER="${commands[less]:-$PAGER}" \
		man "$@"
}

# Interactive search man
fman() {
    name=$(man -k . | fzf --prompt='Man> ' | awk '{print $1}')
	if [ "x$name" != "x" ]
    then
        man $name
    fi
}

# Update zinit and plugins
fupdate() {
    rm -f $ZSH_COMPDUMP
    zinit self-update
    zinit update

    # generate ZSH_COMPDUMP
    compinit -i -C -d "${ZSH_COMPDUMP}"
}
