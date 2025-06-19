if (( $+commands[pacman] )); then
    pacman() {
        local cmd
        for arg in "$@"; do
            case "$arg" in
                -*) cmd="$arg"; break ;;
            esac
        done
        case "$cmd" in
            -S|-U|-R|-D)
                sudo /usr/bin/pacman "$@"
                local ret=$?
                [[ $ret -eq 0 ]] && rehash
                return $ret ;;
            *)
                /usr/bin/pacman "$@" ;;
        esac
    }

    _post_compinit+=("compdef _pacman pacman")
fi

if (( $+commands[yay] )); then
    yay() {
        local cmd
        for arg in "$@"; do
            case "$arg" in
                -*) cmd="$arg"; break ;;
            esac
        done

        case "$cmd" in
            -S|-U|-R|-D)
                /usr/bin/yay "$@"
                local ret=$?
                [[ $ret -eq 0 ]] && rehash
                return $ret ;;
            *)
                /usr/bin/yay "$@" ;;
        esac
    }

    _post_compinit+=("compdef _yay yay")
fi

_sctl_get_command() {
    for arg in "$@"; do
        case "$arg" in
            -*) continue ;;
            *) echo "$arg"; return ;;
        esac
    done
}

_sctl_needs_sudo() {
    case "$1" in
        start|stop|restart|reload|enable|disable|mask|unmask|\
        reset-failed|daemon-reload|isolate|kill|set-property|\
        bind|edit|link|revert|set-environment|unset-environment|\
        import-environment|reload-or-restart|try-restart|reexec|\
        suspend|hibernate|poweroff|reboot|halt|switch-root|\
        emergency|rescue)
            return 0 ;;
        *)
            return 1 ;;
    esac
}

sctl() {
    local cmd
    cmd=$(_sctl_get_command "$@")

    if _sctl_needs_sudo "$cmd"; then
        sudo systemctl "$@"
    else
        systemctl "$@"
    fi
}

_post_compinit+=("compdef _systemctl sctl")

alias jctl='sudo journalctl'

_post_compinit+=("compdef _journalctl jctl")
