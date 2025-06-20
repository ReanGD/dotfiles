autoload -Uz compinit

# Run compaudit (heavy check) only once a day.
# If the dump file is newer than 24 hours, load it without check (-C).
if [[ -f "$ZSH_COMPDUMP" && $(find "$ZSH_COMPDUMP" -mmin -1440 2>/dev/null) ]]; then
  compinit -C -d "$ZSH_COMPDUMP"
else
  compinit -d "$ZSH_COMPDUMP"
fi

# defer compdef
for _cmd in "${_post_compinit[@]}"; do
  eval "$_cmd"
done
unset _post_compinit
