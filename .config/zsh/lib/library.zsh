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

# switch to paste keymap
lib-start-paste() {
    bindkey -A paste main
}

# restore rean keymap
lib-end-paste() {
    bindkey -A rean main
    LBUFFER+=$_paste_content
    unset _paste_content
}

# full paste content
lib-paste-insert() {
    _paste_content+=$KEYS
}
