# Edit the current command line in sublime
edit-command-line-sublime () {
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
