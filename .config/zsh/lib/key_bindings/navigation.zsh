# Повторное нажатие не переходит из буфера в историю.
_beginning_of_buffer() { CURSOR=0; }
_end_of_buffer() { CURSOR=${#BUFFER}; }
zle -N _beginning_of_buffer
zle -N _end_of_buffer

() {
  local section='Навигация'
  _bind_with_help Left backward-char '←' 'На символ влево'
  _bind_with_help Right forward-char '→' 'На символ вправо'
  _bind_with_help Ctrl-Left backward-word 'Ctrl+←' 'На слово влево'
  _bind_with_help Ctrl-Right forward-word 'Ctrl+→' 'На слово вправо'
  _bind_with_help Home beginning-of-line 'Home' 'В начало строки'
  _bind_with_help End end-of-line 'End' 'В конец строки'
  _bind_with_help Ctrl-Home _beginning_of_buffer 'Ctrl+Home' 'В начало всего ввода'
  _bind_with_help Ctrl-End _end_of_buffer 'Ctrl+End' 'В конец всего ввода'
}
