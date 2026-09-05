_history_widget() {
  emulate -L zsh
  if (( ! $+commands[fzf] )); then
    zle history-incremental-search-backward
    return
  fi

  zmodload -i zsh/parameter || return
  (( ${#history} )) || return 0
  local selected
  # NUL разделяет команды и сохраняет переводы строк при выборе.
  selected=$(
    print -rNC1 -- "${(@v)history}" |
      fzf ${FZF_COLOR:+"$FZF_COLOR"} --read0 --print0 +m --tiebreak=begin --query="$BUFFER"
  ) || return

  BUFFER=${selected%$'\0'}
  CURSOR=${#BUFFER}
  zle redisplay
}
zle -N _history_widget

() {
  local section='История'
  # В многострочном вводе эти виджеты сначала перемещаются по строкам.
  _bind_with_help Alt-I up-line-or-search 'Alt+I' 'Строка выше / прошлая команда по первому слову' Alt-I-RU
  _bind_with_help Up up-line-or-search '↑' 'Строка выше / прошлая команда по первому слову'
  _bind_with_help Alt-K down-line-or-search 'Alt+K' 'Строка ниже / следующая команда по первому слову' Alt-K-RU
  _bind_with_help Down down-line-or-search '↓' 'Строка ниже / следующая команда по первому слову'
  _bind_with_help PageUp up-line-or-history 'PageUp' 'Строка выше / прошлая команда без фильтра'
  _bind_with_help PageDown down-line-or-history 'PageDown' 'Строка ниже / следующая команда без фильтра'

  if (( $+widgets[atuin-search] )); then
    _bind_with_help Ctrl-R atuin-search 'Ctrl+R' 'Поиск истории через Atuin' Ctrl-R-RU
    if (( $+widgets[atuin-search-directory] )); then
      _bind_with_help Alt-Ctrl-R atuin-search-directory 'Alt+Ctrl+R' 'Поиск истории текущего каталога' Alt-Ctrl-R-RU
    fi
  else
    local search='Поиск истории через Zsh'
    (( $+commands[fzf] )) && search='Поиск истории через fzf'
    _bind_with_help Ctrl-R _history_widget 'Ctrl+R' "$search" Ctrl-R-RU
  fi
}
