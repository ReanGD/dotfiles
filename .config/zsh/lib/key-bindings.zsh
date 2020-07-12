# list of keymaps: bindkey -l
# hotkeys in keymap: bindkey -M rean
# ^[u = alt+u
# ^U = ctrl+U
# for get key code - exec "cat > /dev/null" and press keys:

bindkey -N rean emacs
bindkey -A rean main

# create a zkbd compatible hash
typeset -g -A key
key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[Control-Left]="^[Od"
key[Control-Right]="^[Oc"

key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"

# Move
bindkey '^[l' forward-char
bindkey "${key[Right]}" forward-char

bindkey '^[j' backward-char
bindkey "${key[Left]}" backward-char

bindkey '^[u' beginning-of-line
bindkey "${key[Home]}" beginning-of-line

bindkey '^[o' end-of-line
bindkey "${key[End]}" end-of-line

bindkey '^L' forward-word
bindkey "${key[Control-Right]}" forward-word

bindkey '^J' backward-word
bindkey "${key[Control-Left]}" backward-word
