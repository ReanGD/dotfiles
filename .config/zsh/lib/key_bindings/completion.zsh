() {
  local section='Дополнение'
  _bind_with_help Tab expand-or-complete 'Tab' 'Дополнить ввод'
  _bind_with_help Shift-Tab reverse-menu-complete 'Shift+Tab' 'Предыдущий вариант дополнения'
  if (( $+commands[tv] && $+widgets[tv-smart-autocomplete] )); then
    _bind_with_help Ctrl-Space tv-smart-autocomplete 'Ctrl+Space' 'Подбор через Television'
  fi
  local keymap=menuselect
  _bind_with_help Esc send-break 'Esc' 'Выйти из меню дополнения'
}
