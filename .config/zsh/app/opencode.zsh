_opencode() {
  _with_proxy env \
  OPENCODE_ENABLE_EXA=1 \
  OPENCODE_EXPERIMENTAL_LSP_TOOL=true \
  opencode "$@"
}

alias opencode='_opencode'
alias oc='_opencode'
