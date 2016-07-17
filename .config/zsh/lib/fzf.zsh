if [[ $- == *i* ]]; then

FZF_COLOR="--color=fg:-1,bg:-1,hl:33,fg+:254,bg+:235,hl+:33,info:136,prompt:136,pointer:230,marker:230,spinner:136"

# CTRL-R - Paste the selected command from history into the command line
fzf-history-widget() {
  LBUFFER=$(fc -lnr 1 | fzf "$FZF_COLOR" --tiebreak=begin)
  zle redisplay
}

zle -N fzf-history-widget
bindkey '^R' fzf-history-widget

# CTRL-D - cd into the bookmarked directory
function fzf-bookmarks-widget() {
  cd $(cat "$HOME/.config/zsh/settings/bkd.cfg" | fzf "$FZF_COLOR" --tiebreak=begin --tac | awk '{print $2}')
  zle reset-prompt
}

zle -N fzf-bookmarks-widget
bindkey '^D' fzf-bookmarks-widget

# fkill - select and kill process
function fkill() {
  pid=$(ps -ef | sed 1d | fzf "$FZF_COLOR" -m | awk '{print $2}')
  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}



# CTRL-F - Paste the selected file path(s) into the command line
__fsel() {
  local cmd="${FZF_CTRL_T_COMMAND:-"command find -L . \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | sed 1d | cut -b3-"}"
  eval "$cmd" | fzf -m | while read item; do
    printf '%q ' "$item"
  done
  echo
}

fzf-file-widget() {
  LBUFFER="${LBUFFER}$(__fsel)"
  zle redisplay
}
zle     -N   fzf-file-widget
bindkey '^F' fzf-file-widget

# cd into the selected directory
fzf-cd-widget() {
  local cmd="${FZF_ALT_C_COMMAND:-"command find -L . \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
    -o -type d -print 2> /dev/null | sed 1d | cut -b3-"}"
  cd "${$(eval "$cmd" | fzf +m):-.}"
  zle reset-prompt
}
zle     -N   fzf-cd-widget
bindkey '^S' fzf-cd-widget

fi
