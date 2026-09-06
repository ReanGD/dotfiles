# Запускается в текущей панели, без загрузки пользовательского .zshrc.
if [[ ! -t 0 || ! -t 1 || -z ${_ZSH_KEYBINDINGS_HELP-} ]]; then
  print -u2 -r -- 'Просмотр справки запускается через F1 в панели Zsh.'
  exit 1
fi

# Режим клавиатуры привязан к экрану: сначала входим в alternate screen.
# Esc кодируется отдельно от префикса стрелок, без таймаутов.
# В help.lesskey учтены NumLock/CapsLock: они меняют коды даже обычных стрелок.
print -rn -- $'\e[?1049h\e[>1u' > /dev/tty
TRAPEXIT() {
  print -rn -- $'\e[<u\e[?1049l' > /dev/tty
}

# Не наследуем пользовательские опции/препроцессоры less.
export LESS= LESSOPEN= LESSCLOSE=
print -r -- "$_ZSH_KEYBINDINGS_HELP" |
  command less --no-init --no-lessopen --RAW-CONTROL-CHARS \
    --lesskey-src="${0:A:h}/help.lesskey" \
    --prompt='↑↓ / PgUp / PgDn · / поиск · Esc / q закрыть'
