# Реестр принадлежит текущей оболочке: просмотрщик получает готовый текст.
_keybindings_help_render() {
  emulate -L zsh
  local previous='' section label description
  local -i i width=0
  for (( i=2; i <= ${#_keybindings_help_entries}; i+=3 )); do
    label=${_keybindings_help_entries[i]}
    (( ${#label} > width )) && width=${#label}
  done
  printf '\e[1;36m  Zsh · Горячие клавиши\e[0m\n\n'
  print -r -- '  ↑↓ / PgUp / PgDn — прокрутка · / — поиск · Esc / q — закрыть'
  print -r -- '  (RU) — также на той же физической клавише в русской раскладке.'
  print -r -- '  Расширенные сочетания и Ctrl+(RU) зависят от протокола клавиатуры терминала.'
  for (( i=1; i <= ${#_keybindings_help_entries}; i+=3 )); do
    section=${_keybindings_help_entries[i]}
    label=${_keybindings_help_entries[i+1]}
    description=${_keybindings_help_entries[i+2]}
    if [[ $section != $previous ]]; then
      printf '\n\e[1;36m  %s\e[0m\n' "$section"
      previous=$section
    fi
    printf '  \e[33m%s\e[0m  %s\n' "${(r:$width:)label}" "$description"
  done
  printf '\n'
}

_keybindings_help_message() {
  if zle; then
    zle -M "$1"
  else
    print -u2 -r -- "$1"
  fi
}

_keybindings_help() {
  emulate -L zsh
  # KITTY_PID — только признак исходного окружения, не адрес живого клиента.
  # В Herdr PID и сокет могут устареть после переподключения.
  if [[ -z ${KITTY_PID-} || -n ${SSH_CONNECTION-} || -n ${SSH_TTY-} ]]; then
    _keybindings_help_message 'Справка рассчитана на локальный kitty, в том числе через Herdr.'
    return 1
  fi
  if (( ! _zle_keyboard_supported )); then
    _keybindings_help_message 'Терминал не подтвердил kitty keyboard protocol; справка не открыта.'
    return 1
  fi
  if (( ! $+commands[less] || ! $+commands[zsh] )); then
    _keybindings_help_message 'Для справки нужны less и zsh.'
    return 1
  fi
  local -i in_zle=0 result
  if zle; then
    in_zle=1
    zle -I
  fi
  # Внутри ZLE стандартный ввод не обязан быть терминалом.
  _ZSH_KEYBINDINGS_HELP="$(_keybindings_help_render)" \
    "${commands[zsh]}" -f "$_keybindings_dir/viewer.zsh" < /dev/tty > /dev/tty
  result=$?
  (( in_zle )) && zle redisplay
  return $result
}
zle -N _keybindings_help

() {
  local section='Справка'
  _bind_with_help F1 _keybindings_help 'F1' 'Открыть справку в текущей панели'
}
