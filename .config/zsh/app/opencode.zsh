_opencode() {
  _with_proxy_ai env \
    LINK_BROWSER=chromium_ai \
    LINK_FILTER=all \
    OPENCODE_ENABLE_EXA=1 \
    OPENCODE_EXPERIMENTAL_LSP_TOOL=true \
    opencode "$@"
}

alias opencode='_opencode'
alias oc='_opencode'
