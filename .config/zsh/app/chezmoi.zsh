_cz_status() {
    local tracked_dirs=(
        "$HOME/.config"
    )
    local red='\033[0;31m'
    local reset='\033[0m'

    echo "Changed files:"
    chezmoi status | while read -r cz_mode cz_path; do
        if [[ -n "$cz_path" ]]; then
            echo -e "  ${red}${cz_mode} ${HOME}/${cz_path#/}${reset}"
        fi
    done
    echo ""
    echo "Untracked files:"
    chezmoi unmanaged "${tracked_dirs[@]}" | while read -r line; do
        if [[ -n "$line" ]]; then
            echo -e "  ${red}${HOME}/${line#/}${reset}"
        fi
    done
}

alias cz=chezmoi
alias czs=_cz_status
