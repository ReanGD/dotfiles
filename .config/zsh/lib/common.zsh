# fix backspace in ssh
TERM=xterm-256color

PAGER='less'
LESS='-R'

FZF_COLOR="--color=fg:-1,bg:-1,hl:33,fg+:254,bg+:235,hl+:33,info:136,prompt:136,pointer:230,marker:230,spinner:136"

# echo "hello" | copy
# copy file.txt
# cat file.txt | copy
copy() {
  if [[ $# -eq 0 ]]; then
    wl-copy
  else
    if [[ ! -f "$1" ]]; then
      echo "Error: '$1' is not a file"
      return 1
    fi
    wl-copy < "$1"
  fi
}
