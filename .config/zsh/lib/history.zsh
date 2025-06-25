# =============================================================================
# History
# =============================================================================

HISTFILE="$ZSH_DATA_DIR/zsh_history"
HISTSIZE=20000
SAVEHIST=20000
HISTORY_IGNORE='(ls|ll|la|pwd|history|exit|clear|cd|cd ..|cd -|ps|ps aux|top|htop|df|du *|free|uname *|whoami|date|time|gst|g df|python|python3)'

mkdir -p "$(dirname "$HISTFILE")"

setopt BANG_HIST              # Поддержка '!' (например !!)
setopt EXTENDED_HISTORY       # Писать timestamp и длительность команды в HISTFILE
setopt INC_APPEND_HISTORY     # Писать в HISTFILE сразу, не ждать закрытия сессии
setopt HIST_EXPIRE_DUPS_FIRST # При переполнении удалять дубликаты первыми
setopt HIST_IGNORE_ALL_DUPS   # Удалять старые вхождения команды при добавлении дубликата
setopt HIST_IGNORE_SPACE      # Не писать команды начинающиеся с пробела
setopt HIST_REDUCE_BLANKS     # Убирать лишние пробелы перед записью
setopt HIST_VERIFY            # Показывать команду после подстановки из истории, не выполнять сразу
setopt HIST_FCNTL_LOCK        # Надёжная блокировка файла истории через fcntl()
setopt HIST_SAVE_NO_DUPS      # Не писать дубликаты в HISTFILE при сохранении
setopt NO_SHARE_HISTORY       # Не шарить историю между терминалами — Atuin делает это сам
setopt NO_APPEND_HISTORY      # Отключаем — используем INC_APPEND_HISTORY
setopt NO_HIST_FIND_NO_DUPS   # Не нужно — дубликатов нет благодаря HIST_IGNORE_ALL_DUPS
setopt NO_HIST_IGNORE_DUPS    # Избыточно при HIST_IGNORE_ALL_DUPS, который полностью покрывает эту логику
setopt NO_HIST_BEEP           # Не бипать при достижении конца истории

## History wrapper — нормализует вывод fc -l
history() {
  [[ ${@[-1]-} = *[0-9]* ]] && builtin fc -l "$@" || builtin fc -l "$@" 1
}
