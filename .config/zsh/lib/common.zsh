# TERM наследуем от терминала или мультиплексора, в том числе через SSH.

export PAGER=less LESS=-R

FZF_COLOR="--color=fg:-1,bg:-1,hl:33,fg+:254,bg+:235,hl+:33,info:136,prompt:136,pointer:230,marker:230,spinner:136"

# Antidote updates itself and all plugins; new shells load the updated plugins.
zsh_update() {
  antidote update "$@"
}

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

