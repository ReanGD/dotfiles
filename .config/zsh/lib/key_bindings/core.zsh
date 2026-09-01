# Общая карта, последовательности и реестр справки.
zmodload -i zsh/terminfo
# ─── Модули ───────────────────────────────────────────────────────────────────
zmodload -i zsh/complist  # Меню completion и карта menuselect.

# ─── Собственная карта клавиш ──────────────────────────────────────────────────
# Начинаем с пустой карты: действия назначаем явно.
bindkey -N rean
bindkey -M rean -R " "-"~"        self-insert  # Печатный ASCII.
bindkey -M rean -R "\M-^@"-"\M-^?" self-insert  # Байты 0x80–0xff, не сочетания с Alt.

bindkey -A rean main  # Используем rean как основную карту ZLE.

# ─── Последовательности клавиш ─────────────────────────────────────────────────
# Специальные клавиши берём из terminfo для текущего TERM.
# Возможности могут отсутствовать: перед привязкой проверяем непустое значение.
typeset -g -A key

key[Esc]=$'\e'
key[Tab]=$'\t'
key[Enter]=$'\r'
key[Shift-Enter]=$'\e[13;2u'  # В legacy-режиме kitty передаёт её через base.conf.
key[Shift-Tab]="${terminfo[kcbt]}"

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Ctrl-Home]="${terminfo[kHOM5]}"
key[Ctrl-End]="${terminfo[kEND5]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"

key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[Ctrl-Left]="${terminfo[kLFT5]}"
key[Ctrl-Right]="${terminfo[kRIT5]}"

key[Delete]="${terminfo[kdch1]}"
key[Ctrl-Delete]="${terminfo[kDC5]}"
key[Backspace]="${terminfo[kbs]}"

# Начало пакетной вставки; завершающую последовательность читает bracketed-paste.
key[Start-Paste]=$'\e[200~'

key[Ctrl-C]=$'\x03'
key[Ctrl-W]=$'\x17'
key[Ctrl-K]=$'\x0b'
key[Ctrl-Z]=$'\x1a'
key[Ctrl-R]=$'\x12'
key[Ctrl-Space]=$'\0'
key[Alt-Ctrl-R]=$'\e\x12'
key[Alt-I]=$'\ei'
key[Alt-I-RU]=$'\eш'
key[Alt-K]=$'\ek'
key[Alt-K-RU]=$'\eл'
key[Alt-Dot]=$'\e.'
key[Alt-Dot-RU]=$'\eю'

# Отдельные CSI-u сочетания без legacy-эквивалента.
key[Ctrl-C-RU]=$'\e[1089;5u'
key[Ctrl-W-RU]=$'\e[1094;5u'
key[Ctrl-K-RU]=$'\e[1083;5u'
key[Ctrl-Z-RU]=$'\e[1103;5u'
key[Ctrl-R-RU]=$'\e[1082;5u'
key[Alt-Ctrl-R-RU]=$'\e[1082;7u'
key[Ctrl-Shift-K]=$'\e[107;6u'
key[Ctrl-Shift-K-RU]=$'\e[1083;6u'
key[Ctrl-Shift-Z]=$'\e[122;6u'
key[Ctrl-Shift-Z-RU]=$'\e[1103;6u'
key[Ctrl-Shift-Backspace]=$'\e[127;6u'
key[F1]="${terminfo[kf1]}"

# Плоские тройки: раздел, сочетание, описание. При повторном source сбрасываем.
typeset -ga _keybindings_help_entries
_keybindings_help_entries=()

# Вызывается внутри () { local section='…'; … }.
# Необязательный local keymap='menuselect' меняет карту с rean на меню.
# Аргументы: имя из key, виджет, отображение, описание, имена альтернатив.
_bind_with_help() {
  emulate -L zsh
  local name=$1 widget=$2 label=$3 description=$4 alias_name
  shift 4
  [[ -n ${key[$name]} ]] || return 0
  bindkey -M "${keymap:-rean}" "${key[$name]}" "$widget" || return
  for alias_name in "$@"; do
    [[ -n ${key[$alias_name]} ]] || continue
    bindkey -M "${keymap:-rean}" "${key[$alias_name]}" "$widget" || return
    [[ $alias_name == *-RU ]] && label=${label%' (RU)'}' (RU)'
  done
  _keybindings_help_entries+=("$section" "$label" "$description")
}
