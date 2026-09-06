# Привязки и справка: добавляйте действие через _bind_with_help в своей секции.
# F1 временно занимает текущую панель локального kitty / Herdr; сокет не нужен.
# Требуется подтверждённый keyboard protocol. ↑↓ / PgUp / PgDn — прокрутка,
# / — поиск, Esc / q — закрыть и вернуться к прежнему вводу.
# (RU) отмечает зарегистрированный вариант для русской раскладки.
# F1 — виджет ZLE: в запущенных приложениях клавиша остаётся приложению.
typeset -g _keybindings_dir="${${(%):-%N}:A:h}/key_bindings"

source "$_keybindings_dir/core.zsh"
source "$_keybindings_dir/navigation.zsh"
source "$_keybindings_dir/editing.zsh"
source "$_keybindings_dir/completion.zsh"
source "$_keybindings_dir/history.zsh"
source "$_keybindings_dir/help.zsh"
# Последним: альтернативные кодировки копируют уже назначенные действия.
source "$_keybindings_dir/terminal.zsh"
