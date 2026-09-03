_insert_newline() {
  LBUFFER+=$'\n'
}
zle -N _insert_newline

() {
  local section='Ввод'
  _bind_with_help Enter accept-line 'Enter' 'Выполнить команду'
  _bind_with_help Ctrl-C send-break 'Ctrl+C' 'Отменить ввод' Ctrl-C-RU
  _bind_with_help Shift-Enter _insert_newline 'Shift+Enter' 'Вставить новую строку без выполнения'
  # Буфер обмена вставляет терминал; ZLE обрабатывает вставку целиком.
  _bind_with_help Start-Paste bracketed-paste 'Вставка терминала' 'Вставить текст целиком (bracketed paste)'

  section='Редактирование'
  _bind_with_help Backspace backward-delete-char 'Backspace' 'Удалить символ слева'
  _bind_with_help Delete delete-char 'Delete' 'Удалить символ справа'
  # Удалённые слова и строки сохраняются в kill ring.
  _bind_with_help Ctrl-W backward-kill-word 'Ctrl+W / Ctrl+Backspace' 'Удалить слово слева' Ctrl-W-RU
  _bind_with_help Ctrl-Delete kill-word 'Ctrl+Delete' 'Удалить слово справа'
  _bind_with_help Ctrl-K kill-line 'Ctrl+K' 'Удалить до конца строки' Ctrl-K-RU
  _bind_with_help Ctrl-Shift-K kill-whole-line 'Ctrl+Shift+K' 'Удалить строку целиком' Ctrl-Shift-K-RU
  _bind_with_help Ctrl-Shift-Backspace kill-buffer 'Ctrl+Shift+Backspace' 'Удалить весь ввод, включая несколько строк'
  _bind_with_help Ctrl-Z undo 'Ctrl+Z' 'Отменить редактирование' Ctrl-Z-RU
  _bind_with_help Ctrl-Shift-Z redo 'Ctrl+Shift+Z' 'Повторить редактирование' Ctrl-Shift-Z-RU
  _bind_with_help Alt-Dot insert-last-word 'Alt+.' 'Вставить последний аргумент прошлой команды' Alt-Dot-RU
}
