# see all commands: https://jlk.fjfi.cvut.cz/arch/manpages/man/zshzle.1#STANDARD_WIDGETS
# list of keymaps: bindkey -l
# hotkeys in keymap: bindkey -M rean
# ^[u = alt+u
# ^U = ctrl+U
# for get key code - exec "cat > /dev/null" and press keys:

# Make sure that the terminal is in application mode when zle is active, since
# only then values from $terminfo are valid
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-line-init() {
    echoti smkx
  }
  function zle-line-finish() {
    echoti rmkx
  }
  zle -N zle-line-init
  zle -N zle-line-finish
fi

source $ZSH_LIB_DIR/key_bindings_funcs.zsh

bindkey -N paste
bindkey -N rean emacs
bindkey -A rean main

# create a zkbd compatible hash
GLOBAL_TERM=$TERM
TERM=rxvt-unicode-256color
typeset -g -A key
key[Tab]="\t"
key[Shift-Tab]="${terminfo[kcbt]}"

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"

key[Delete]="${terminfo[kdch1]}"
key[Ctrl-Delete]="^[[3^"
key[Backspace]="${terminfo[kbs]}"
key[Ctrl-Backspace]="^[[33~"

key[Left]="${terminfo[kcub1]}"
key[Ctrl-Left]="^[Od"
key[Right]="${terminfo[kcuf1]}"
key[Ctrl-Right]="^[Oc"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"

key[Start-Paste]="^[[200~"
key[End-Paste]="^[[201~"

TERM=$GLOBAL_TERM
unset GLOBAL_TERM

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
bindkey "${key[Ctrl-Right]}" forward-word

bindkey '^J' backward-word
bindkey "${key[Ctrl-Left]}" backward-word


# Edit
bindkey '^[f' delete-char
bindkey "${key[Delete]}" delete-char

bindkey '^[d' backward-delete-char
bindkey "${key[Backspace]}" backward-delete-char

bindkey '^[r' delete-word
bindkey "${key[Ctrl-Delete]}" delete-word

bindkey '^[e' backward-delete-word
bindkey "${key[Ctrl-Backspace]}" backward-delete-word

bindkey '^[g' kill-whole-line

bindkey '^[t' clear-screen

zle -N _edit_command_line_sublime
bindkey '^X^E' _edit_command_line_sublime


# History
bindkey '^[i' up-line-or-search
bindkey "${key[Up]}" up-line-or-search

bindkey '^[k' down-line-or-search
bindkey "${key[Down]}" down-line-or-search

bindkey "${key[PageUp]}" up-line-or-history
bindkey "${key[PageDown]}" down-line-or-history

bindkey "${key[Tab]}" menu-expand-or-complete
bindkey "${key[Shift-Tab]}" reverse-menu-complete

zle -N _history_widget
bindkey '^R' _history_widget


# Widgets
zle -N _bookmarks_widget
bindkey '^D' _bookmarks_widget

zle -N _file_widget
bindkey '^F' _file_widget

# Paste mode
# see: https://github.com/zsh-users/zsh-autosuggestions/issues/141#issuecomment-280876210
zle -N _start_paste
bindkey "${key[Start-Paste]}" _start_paste

zle -N _end_paste
bindkey -M paste "${key[End-Paste]}" _end_paste

zle -N paste-insert _paste_insert
bindkey -R -M paste "^@"-"\M-^?" paste-insert
bindkey -M paste -s '^M' '^J'
