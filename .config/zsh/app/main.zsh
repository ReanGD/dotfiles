# Usage: _with_proxy_endpoint HOST PORT COMMAND [ARG...]; pass IPv6 hosts without brackets.
# Environment hints only: HTTP(S), SOCKS with remote DNS, and JVM HTTP(S) properties.
# Existing JVM options stay first; this endpoint wins over earlier proxy properties.
_with_proxy_endpoint() {
  if (( $# < 3 )); then
    print -u2 -- 'Usage: _with_proxy_endpoint HOST PORT COMMAND [ARG...]'
    return 2
  fi
  local proxy_host="$1" proxy_port="$2"
  shift 2
  local proxy_url_host="$proxy_host"
  [[ "$proxy_host" == *:* ]] && proxy_url_host="[$proxy_host]"
  local proxy_http="http://${proxy_url_host}:${proxy_port}"
  local proxy_socks="socks5h://${proxy_url_host}:${proxy_port}"
  local proxy_bypass='localhost,127.0.0.1,127.0.0.0/8,::1,[::1]'
  local proxy_java="-Dhttp.proxyHost=${proxy_host} -Dhttp.proxyPort=${proxy_port} -Dhttps.proxyHost=${proxy_host} -Dhttps.proxyPort=${proxy_port}"

  http_proxy="$proxy_http" \
  https_proxy="$proxy_http" \
  HTTP_PROXY="$proxy_http" \
  HTTPS_PROXY="$proxy_http" \
  all_proxy="$proxy_socks" \
  ALL_PROXY="$proxy_socks" \
  JAVA_OPTS="${JAVA_OPTS:+${JAVA_OPTS} }${proxy_java}" \
  JAVA_TOOL_OPTIONS="${JAVA_TOOL_OPTIONS:+${JAVA_TOOL_OPTIONS} }${proxy_java}" \
  no_proxy="$proxy_bypass" \
  NO_PROXY="$proxy_bypass" \
  "$@"
}

_with_proxy() {
  _with_proxy_endpoint 127.0.0.1 10202 "$@"
}

_with_proxy_ai() {
  _with_proxy_endpoint 127.0.0.1 10301 "$@"
}

_source_if_exists() {
  local cmd="$1"
  local file="$2"
  if command -v "$cmd" &>/dev/null; then
    source "$file"
  fi
}

typeset -ga _post_compinit
typeset _omp_bin="${XDG_CACHE_HOME:-$HOME/.cache}/.bun/bin/omp"

source $ZSH_APP_DIR/_system.zsh
source $ZSH_APP_DIR/_aliases.zsh
_source_if_exists fd $ZSH_APP_DIR/fd.zsh
_source_if_exists git $ZSH_APP_DIR/git.zsh
_source_if_exists opencode $ZSH_APP_DIR/opencode.zsh
_source_if_exists "$_omp_bin" "$ZSH_APP_DIR/omp.zsh"
_source_if_exists eza $ZSH_APP_DIR/eza.zsh
_source_if_exists rg $ZSH_APP_DIR/rg.zsh
unset _omp_bin

eval "$(mise activate zsh)"
