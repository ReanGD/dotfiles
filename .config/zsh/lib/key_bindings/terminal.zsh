# ─── Режим клавиатуры терминала ────────────────────────────────────────────────
# На время ZLE включаем режим клавиш, соответствующий terminfo.
autoload -Uz add-zle-hook-widget

if [[ -n ${terminfo[smkx]} && -n ${terminfo[rmkx]} ]]; then
  _zle_keypad_on() {
    echoti smkx
  }

  _zle_keypad_off() {
    echoti rmkx
  }

  # Добавляем обработчики, сохраняя существующие ZLE-хуки.
  add-zle-hook-widget line-init _zle_keypad_on
  add-zle-hook-widget line-finish _zle_keypad_off
fi

# Проверяем протокол один раз, уже в ZLE: прочитанный пользовательский ввод
# возвращаем редактору. Без ответа за 150 мс остаётся обычный режим.
typeset -gi _zle_keyboard_checked _zle_keyboard_supported _zle_keyboard_active
autoload -Uz add-zsh-hook
zmodload -i zsh/datetime

_zle_keyboard_probe() {
  emulate -L zsh
  setopt nomultibyte
  _zle_keyboard_checked=1
  [[ -n $TTY && $TERM != dumb ]] || return 0

  local pending='' character MATCH MBEGIN MEND
  local -a match mbegin mend
  local -F deadline=$(( EPOCHREALTIME + 0.15 )) remaining
  local pattern=$'\e\\[\\?([0-9]+)u'
  print -rn -- $'\e[?u'
  while (( (remaining = deadline - EPOCHREALTIME) > 0 )); do
    read -r -k 1 -t "$remaining" character || break
    pending+=$character
    if [[ $pending =~ $pattern ]]; then
      _zle_keyboard_supported=1
      pending=${pending/"$MATCH"/}
      break
    fi
  done
  [[ -z $pending ]] || zle -U "$pending"
  return 0
}

_zle_keyboard_on() {
  (( _zle_keyboard_checked )) || _zle_keyboard_probe
  (( _zle_keyboard_supported && ! _zle_keyboard_active )) || return 0
  # Push/pop сохраняют прежний режим, в том числе уже включённый протокол.
  print -rn -- $'\e[>1u'
  _zle_keyboard_active=1
}

_zle_keyboard_off() {
  (( _zle_keyboard_active )) || return 0
  print -rn -- $'\e[<u'
  _zle_keyboard_active=0
}

# Запоздалый ответ обрабатываем как служебный, а не как текст команды.
_zle_keyboard_reply() {
  _zle_keyboard_supported=1
  _zle_keyboard_on
}
zle -N _zle_keyboard_reply
add-zle-hook-widget line-init _zle_keyboard_on
add-zle-hook-widget line-finish _zle_keyboard_off
# Ctrl+C пропускает line-finish; перед следующим prompt убираем режим.
add-zsh-hook precmd _zle_keyboard_off

# ─── Расширенный ввод ──────────────────────────────────────────────────────────
# Принимаем оба кодирования: действия общие, режим выбирает ответ терминала.
() {
  local name sequence flags
  local -a binding
  local -A sequences=(
    Esc $'\e[27u'
    Up $'\e[A' Down $'\e[B' Left $'\e[D' Right $'\e[C'
    Home $'\e[H' End $'\e[F'
    F1 $'\e[P'
    Ctrl-C $'\e[99;5u' Ctrl-W $'\e[119;5u' Ctrl-K $'\e[107;5u'
    Ctrl-Z $'\e[122;5u' Ctrl-R $'\e[114;5u' Ctrl-Space $'\e[32;5u'
    Alt-Ctrl-R $'\e[114;7u'
    Alt-I $'\e[105;3u' Alt-I-RU $'\e[1096;3u'
    Alt-K $'\e[107;3u' Alt-K-RU $'\e[1083;3u'
    Alt-Dot $'\e[46;3u' Alt-Dot-RU $'\e[1102;3u'
  )
  for name sequence in "${(@kv)sequences}"; do
    [[ -n ${key[$name]} ]] || continue
    binding=(${(z)"$(bindkey -M rean "${key[$name]}")"})
    [[ ${binding[2]} != undefined-key ]] || continue
    bindkey -M rean "$sequence" "${binding[2]}"
  done
  bindkey -M menuselect "${sequences[Esc]}" send-break

  # Пять флагов протокола дают значения 0–31; 0 тоже подтверждает поддержку.
  for flags in {0..31}; do
    bindkey -M rean $'\e[?'"$flags"u _zle_keyboard_reply
  done
}
