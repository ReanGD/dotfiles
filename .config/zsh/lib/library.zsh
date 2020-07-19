# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Edit the current command line in sublime
# for see origin:
# autoload -z edit-command-line
# zle -N edit-command-line
# edit-command-line
# whence -f edit-command-line
lib-edit-command-line-sublime() {
	emulate -L zsh
	() {
		exec < /dev/tty
		setopt localoptions nomultibyte noksharrays
		(( $+zle_bracketed_paste )) && print -r -n - $zle_bracketed_paste[2]
		eval "/usr/bin/subl3 -w $1"
		(( $+zle_bracketed_paste )) && print -r -n - $zle_bracketed_paste[1]
		print -Rz - "$(<$1)"
	} =(<<<"$PREBUFFER$BUFFER")
	zle send-break
}

# Switch to paste keymap
lib-start-paste() {
    bindkey -A paste main
}

# Restore rean keymap
lib-end-paste() {
    bindkey -A rean main
    LBUFFER+=$_paste_content
    unset _paste_content
}

# Full paste content
lib-paste-insert() {
    _paste_content+=$KEYS
}

FZF_COLOR="--color=fg:-1,bg:-1,hl:33,fg+:254,bg+:235,hl+:33,info:136,prompt:136,pointer:230,marker:230,spinner:136"

# Paste the selected command from history into the command line
fzf-history-widget() {
  LBUFFER=$(fc -lnr 1 | fzf "$FZF_COLOR" --tiebreak=begin)
  zle redisplay
}

# cd into the bookmarked directory
fzf-bookmarks-widget() {
  eval cd $(cat "$XDG_CONFIG_HOME/zsh/settings/bkd.cfg" | fzf "$FZF_COLOR" --tiebreak=begin --tac | awk '{print $2}')
  zle reset-prompt
}

# Paste the selected file path(s) into the command line
fzf-file-widget() {
  LBUFFER+=$(fd -H -E .git | fzf -m | while read item; do printf '%q ' "$item";	done )
  zle redisplay
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

# Interactive search man
fman() {
    name=$(man -k . | fzf --prompt='Man> ' | awk '{print $1}')
	if [ "x$name" != "x" ]
    then
        man $name
    fi
}

# Change user in gitconfig
git_change_user() {
	if [[ "$(_git_command config user.name)" == "ReanGD" ]]; then
        cp $XDG_CONFIG_HOME/git/config.work $XDG_CONFIG_HOME/git/config
    else
        cp $XDG_CONFIG_HOME/git/config.home $XDG_CONFIG_HOME/git/config
    fi
}

# Interactive command z
zz() {
  cd "$(_z -l 2>&1 | sed 's/^[0-9,.]* *//' | fzf --tac)"
}
