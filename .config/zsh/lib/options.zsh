setopt AUTO_CD    # goto directory without "cd"
setopt NO_BEEP    # disable beep for incorrect command (or use: unsetopt BEEP)
setopt MULTIOS    # see http://zsh.sourceforge.net/Doc/Release/Redirection.html#Multios
setopt IGNORE_EOF # do not exit from zsh after Control+D

# I just copied this
[[ -n "$WINDOW" ]] && SCREEN_NO="%B$WINDOW%b " || SCREEN_NO=""
