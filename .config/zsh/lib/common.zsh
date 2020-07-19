SHORT_HOST=${HOST/.*/}

_git_command() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}
