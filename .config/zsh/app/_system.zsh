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
