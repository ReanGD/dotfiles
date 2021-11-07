# Edit the current command line in sublime
# for see origin:
# autoload -z edit-command-line
# zle -N edit-command-line
# edit-command-line
# whence -f edit-command-line
_edit_command_line_sublime() {
	emulate -L zsh
	() {
		exec < /dev/tty
		setopt localoptions nomultibyte noksharrays
		(( $+zle_bracketed_paste )) && print -r -n - $zle_bracketed_paste[2]
		eval "/usr/bin/subl -w $1"
		(( $+zle_bracketed_paste )) && print -r -n - $zle_bracketed_paste[1]
		print -Rz - "$(<$1)"
	} =(<<<"$PREBUFFER$BUFFER")
	zle send-break
}

# Switch to paste keymap
_start_paste() {
    bindkey -A paste main
}

# Restore rean keymap
_end_paste() {
    bindkey -A rean main
    LBUFFER+=$_paste_content
    unset _paste_content
}

# Full paste content
_paste_insert() {
    _paste_content+=$KEYS
}

# Paste the selected command from history into the command line
_history_widget() {
  LBUFFER=$(fc -lnr 1 | fzf "$FZF_COLOR" --tiebreak=begin)
  zle redisplay
}

# cd into the bookmarked directory
_bookmarks_widget() {
  eval cd $(cat "$XDG_CONFIG_HOME/zsh/settings/bkd.cfg" | fzf "$FZF_COLOR" --tiebreak=begin --tac | awk '{print $2}')
  zle reset-prompt
}

# Paste the selected file path(s) into the command line
_file_widget() {
  LBUFFER+=$(fd -H -E .git | fzf -m | while read item; do printf '%q ' "$item";	done )
  zle redisplay
}
