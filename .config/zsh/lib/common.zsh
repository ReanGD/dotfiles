SHORT_HOST=${HOST/.*/}

_git_command() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}

# fix backspace in ssh
TERM=xterm-256color

FZF_COLOR="--color=fg:-1,bg:-1,hl:33,fg+:254,bg+:235,hl+:33,info:136,prompt:136,pointer:230,marker:230,spinner:136"
