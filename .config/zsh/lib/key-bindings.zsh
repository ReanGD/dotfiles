# see all commands: https://jlk.fjfi.cvut.cz/arch/manpages/man/zshzle.1#STANDARD_WIDGETS
# list of keymaps: bindkey -l
# hotkeys in keymap: bindkey -M rean
# ^[u = alt+u
# ^U = ctrl+U
# for get key code - exec "cat > /dev/null" and press keys:

source $ZSH_CUSTOM/lib/library.zsh

bindkey -N paste
bindkey -N rean emacs
bindkey -A rean main

# create a zkbd compatible hash
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

zle -N lib-edit-command-line-sublime
bindkey '^X^E' lib-edit-command-line-sublime


# History
bindkey '^[i' up-line-or-search
bindkey "${key[Up]}" up-line-or-search

bindkey '^[k' down-line-or-search
bindkey "${key[Down]}" down-line-or-search

bindkey "${key[PageUp]}" up-line-or-history
bindkey "${key[PageDown]}" down-line-or-history

bindkey "${key[Tab]}" menu-expand-or-complete
bindkey "${key[Shift-Tab]}" reverse-menu-complete

zle -N fzf-history-widget
bindkey '^R' fzf-history-widget


# Widgets
zle -N fzf-bookmarks-widget
bindkey '^D' fzf-bookmarks-widget

zle -N fzf-file-widget
bindkey '^F' fzf-file-widget

# Paste mode
# see: https://github.com/zsh-users/zsh-autosuggestions/issues/141#issuecomment-280876210
zle -N lib-start-paste
bindkey "${key[Start-Paste]}" lib-start-paste

zle -N lib-end-paste
bindkey -M paste "${key[End-Paste]}" lib-end-paste

zle -N paste-insert lib-paste-insert
bindkey -R -M paste "^@"-"\M-^?" paste-insert
bindkey -M paste -s '^M' '^J'


# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	autoload -Uz add-zle-hook-widget
	function zle_application_mode_start { echoti smkx }
	function zle_application_mode_stop { echoti rmkx }
	add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi
