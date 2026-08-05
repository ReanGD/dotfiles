_cz_status() {
    emulate -L zsh

    local tracked_dirs=(
        "$HOME/.config"
        "$HOME/.local/share/applications"
    )

    local red=$'\e[0;31m'
    local yellow=$'\e[0;33m'
    local bold=$'\e[1m'
    local reset=$'\e[0m'

    local changes unmanaged
    changes="$(command chezmoi status --exclude scripts)" || return
    unmanaged="$(command chezmoi unmanaged "${tracked_dirs[@]}")" || return

    if [[ -z "$changes" && -z "$unmanaged" ]]; then
        print -P "%Bnothing to report, chezmoi is clean%b"
        return 0
    fi

    if [[ -n "$changes" ]]; then
        local section="" line="" printed=0
        local last_written_status="" target_status="" rel=""
        local word="" padded="" change_status=""
        for section in local pending; do
            printed=0
            while IFS= read -r line; do
                [[ -z "$line" ]] && continue

                last_written_status="${line:0:1}"
                target_status="${line:1:1}"
                rel="${line:3}"
                rel="${rel#/}"

                if [[ "$section" == local ]]; then
                    [[ "$last_written_status" == " " ]] && continue
                    change_status="$last_written_status"
                else
                    [[ "$last_written_status" != " " || "$target_status" == " " ]] && continue
                    change_status="$target_status"
                fi

                if (( ! printed )); then
                    if [[ "$section" == local ]]; then
                        printf '%sLocal changes:%s\n' "$bold" "$reset"
                        print -r -- '  (chezmoi add <path> to save; chezmoi forget <path> to stop tracking a deleted file)'
                        print -r -- '  (chezmoi diff to review; chezmoi apply can overwrite local changes)'
                    else
                        printf '%sPending changes from chezmoi:%s\n' "$bold" "$reset"
                        print -r -- '  (chezmoi diff to review; chezmoi apply to apply)'
                    fi
                    printed=1
                fi

                case "$change_status" in
                    M) word="modified" ;;
                    A) word="added" ;;
                    D) word="deleted" ;;
                    *) word="changed" ;;
                esac
                printf -v padded '%-11s' "${word}:"
                printf '  %s%s%s %s/%s\n' "$yellow" "$padded" "$reset" "$HOME" "$rel"
            done <<< "$changes"
            (( printed )) && print
        done
    fi

    if [[ -n "$unmanaged" ]]; then
        print -P "${bold}Untracked files:${reset}"
        print -r -- '  (chezmoi add <path> to track; or add a pattern to .chezmoiignore)'
        while IFS= read -r line; do
            [[ -z "$line" ]] && continue
            printf '  %s%s/%s%s\n' "$red" "$HOME" "${line#/}" "$reset"
        done <<< "$unmanaged"
    fi
}

alias cz=chezmoi
alias czs=_cz_status
