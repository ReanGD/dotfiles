_pacman_needs_sudo() {
    emulate -L zsh
    local arg option flag operation=""
    local -i printing=0 reading=0 cleaning=0 refreshing=0 checking=0 sysroot=0

    while (( $# )); do
        arg="$1"
        shift
        case "$arg" in
            --) break ;;
            --help|--version) return 1 ;;
            --database) arg=D ;;
            --files) arg=F ;;
            --query) arg=Q ;;
            --remove) arg=R ;;
            --sync) arg=S ;;
            --deptest) arg=T ;;
            --upgrade) arg=U ;;
            --print) arg=p ;;
            --groups|--info|--list|--search) arg=i ;;
            --clean) arg=c ;;
            --refresh) arg=y ;;
            --check) arg=k ;;
            --debug|--debug=*|--cascade|--changelog|--nodeps|--deps|--explicit|\
            --foreign|--native|--nosave|--owns|--file|--quiet|--recursive|--regex|\
            --machinereadable|--unrequired|--upgrades|--sysupgrade|--unneeded|\
            --verbose|--downloadonly|--noconfirm|--confirm|--force|--noprogressbar|\
            --noscriptlet|--asdeps|--needed|--asexplicit|--dbonly|\
            --disable-download-timeout|--disable-sandbox|\
            --disable-sandbox-filesystem|--disable-sandbox-syscalls)
                continue ;;
            --*)
                option="${arg%%=*}"
                case "$option" in
                    --dbpath|--root|--sysroot|--config|--ignore|--assume-installed|\
                    --overwrite|--ask|--cachedir|--hookdir|--logfile|--ignoregroup|\
                    --arch|--print-format|--gpgdir|--color)
                        if [[ "$arg" != *=* ]]; then
                            (( $# )) || return 1
                            shift
                        fi
                        [[ "$option" == --sysroot ]] && sysroot=1
                        [[ "$option" == --print-format ]] && printing=1
                        continue ;;
                    *) return 1 ;;
                esac ;;
            -?*) arg="${arg#-}" ;;
            *) continue ;;
        esac

        while [[ -n "$arg" ]]; do
            flag="${arg[1]}"
            arg="${arg[2,-1]}"
            case "$flag" in
                D|F|Q|R|S|T|U)
                    [[ -z "$operation" ]] || return 1
                    operation="$flag" ;;
                h|V) return 1 ;;
                b|r)
                    if [[ -z "$arg" ]]; then
                        (( $# )) || return 1
                        shift
                    fi
                    break ;;
                p) printing=1 ;;
                i|g|l|s) reading=1 ;;
                c) cleaning=1 ;;
                y) refreshing=1 ;;
                k) checking=1 ;;
                d|e|f|m|n|o|q|t|u|v|w|x) ;;
                *) return 1 ;;
            esac
        done
    done

    [[ -n "$operation" ]] || return 1
    (( sysroot )) && return 0
    case "$operation" in
        D) (( ! checking )) ;;
        R|U) (( ! printing )) ;;
        S) (( cleaning || refreshing || (! reading && ! printing) )) ;;
        F) (( refreshing )) ;;
        *) return 1 ;;
    esac
}

if (( $+commands[pacman] )); then
    pacman() {
        local ret=0
        if _pacman_needs_sudo "$@"; then
            sudo /usr/bin/pacman "$@" || ret=$?
        else
            /usr/bin/pacman "$@" || ret=$?
        fi
        rehash
        return "$ret"
    }

    _post_compinit+=("compdef _pacman pacman")
fi

if (( $+commands[yay] )); then
    yay() {
        local ret=0
        /usr/bin/yay "$@" || ret=$?
        rehash
        return "$ret"
    }

    _post_compinit+=("compdef _yay yay")
fi

_sctl_needs_sudo() {
    emulate -L zsh
    local arg option flag
    local -a positional=()

    while (( $# )); do
        arg="$1"
        shift
        case "$arg" in
            --)
                positional+=("$@")
                break ;;
            --user|--host|--host=*|--machine|--machine=*|--capsule|--capsule=*|--help|--version)
                return 1 ;;
            --system|--global|--all|--full|--recursive|--reverse|--before|--after|\
            --with-dependencies|--failed|--show-transaction|--show-types|--value|\
            --now|--dry-run|--quiet|--verbose|--no-warn|--wait|--no-block|--no-wall|\
            --no-reload|--no-pager|--no-ask-password|--runtime|--force|\
            --firmware-setup|--kernel-cmdline-reuse|--plain|--read-only|--mkdir|\
            --marked|--stdin)
                ;;
            --*)
                option="${arg%%=*}"
                case "$option" in
                    --type|--state|--property|--job-mode|--check-inhibitors|\
                    --signal|--kill-whom|--kill-who|--kill-value|--kill-subgroup|\
                    --what|--message|--legend|--preset-mode|--root|--image|\
                    --image-policy|--lines|--output|--boot-loader-menu|\
                    --boot-loader-entry|--reboot-argument|--kernel-cmdline|\
                    --timestamp|--drop-in|--when)
                        if [[ "$arg" != *=* ]]; then
                            (( $# )) || return 1
                            shift
                        fi ;;
                    *) return 1 ;;
                esac ;;
            -?*)
                # Короткие опции могут быть сгруппированы: -qf, -qsSIGTERM.
                arg="${arg#-}"
                while [[ -n "$arg" ]]; do
                    flag="${arg[1]}"
                    arg="${arg[2,-1]}"
                    case "$flag" in
                        H|M|C|h) return 1 ;;
                        a|l|r|T|i|q|v|f) ;;
                        t|p|P|s|n|o)
                            if [[ -z "$arg" ]]; then
                                (( $# )) || return 1
                                shift
                            fi
                            break ;;
                        *) return 1 ;;
                    esac
                done ;;
            *) positional+=("$arg") ;;
        esac
    done

    case "${positional[1]}" in
        start|stop|restart|reload|try-restart|reload-or-restart|try-reload-or-restart|\
        enable|disable|reenable|preset|preset-all|mask|unmask|link|revert|\
        add-wants|add-requires|edit|set-default|reset-failed|daemon-reload|\
        daemon-reexec|isolate|kill|clean|freeze|thaw|set-property|bind|mount-image|\
        set-environment|unset-environment|import-environment|enqueue-marked|cancel|\
        default|emergency|rescue|halt|poweroff|reboot|kexec|soft-reboot|exit|\
        switch-root|sleep|suspend|hibernate|hybrid-sleep|suspend-then-hibernate)
            return 0 ;;
        log-level|log-target|service-watchdogs)
            (( ${#positional} > 1 )) ;;
        service-log-level|service-log-target)
            (( ${#positional} > 2 )) ;;
        *) return 1 ;;
    esac
}

sctl() {
    if _sctl_needs_sudo "$@"; then
        sudo systemctl "$@"
    else
        systemctl "$@"
    fi
}

_post_compinit+=("compdef _systemctl sctl")

alias jctl='sudo journalctl'

_post_compinit+=("compdef _journalctl jctl")
