autoload -Uz compinit

# Reuse the dump, but check for new completion files and insecure directories.
compinit -d "$ZSH_COMPDUMP"

# defer compdef
for _cmd in "${_post_compinit[@]}"; do
  eval "$_cmd"
done
unset _post_compinit
