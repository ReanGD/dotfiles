setopt NO_BEEP               # disable beep for incorrect command (or use: unsetopt BEEP)
setopt MULTIOS               # see http://zsh.sourceforge.net/Doc/Release/Redirection.html#Multios
setopt IGNORE_EOF            # do not exit from zsh after Control+D
setopt LONG_LIST_JOBS        # print job notifications in the long format by default.
setopt INTERACTIVE_COMMENTS  # enable support comments

# I just copied this
[[ -n "$WINDOW" ]] && SCREEN_NO="%B$WINDOW%b " || SCREEN_NO=""
