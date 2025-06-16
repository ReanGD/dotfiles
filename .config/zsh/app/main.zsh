_PROXY_HOST="127.0.0.1"
_PROXY_PORT="10202"

_with_proxy() {
  http_proxy="http://${_PROXY_HOST}:${_PROXY_PORT}" \
  https_proxy="http://${_PROXY_HOST}:${_PROXY_PORT}" \
  HTTP_PROXY="http://${_PROXY_HOST}:${_PROXY_PORT}" \
  HTTPS_PROXY="http://${_PROXY_HOST}:${_PROXY_PORT}" \
  all_proxy="socks5://${_PROXY_HOST}:${_PROXY_PORT}" \
  ALL_PROXY="socks5://${_PROXY_HOST}:${_PROXY_PORT}" \
  JAVA_OPTS="-Dhttp.proxyHost=${_PROXY_HOST} -Dhttp.proxyPort=${_PROXY_PORT} -Dhttps.proxyHost=${_PROXY_HOST} -Dhttps.proxyPort=${_PROXY_PORT}" \
  no_proxy="localhost,127.0.0.0/8,::1" \
  NO_PROXY="localhost,127.0.0.0/8,::1" \
  "$@"
}

_source_if_exists() {
  local cmd="$1"
  local file="$2"
  if command -v "$cmd" &>/dev/null; then
    source "$file"
  fi
}

_source_if_exists fd $ZSH_APP_DIR/fd.zsh
_source_if_exists chezmoi $ZSH_APP_DIR/chezmoi.zsh
_source_if_exists opencode $ZSH_APP_DIR/opencode.zsh

