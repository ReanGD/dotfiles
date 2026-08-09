# Искать в скрытых файлах, следовать ссылкам
alias rg='command rg --hidden --follow --glob="!.git/"'

# Аналог rg, но искать без скрытых файлов
alias rgnh='command rg --no-hidden --follow --glob="!.git/"'

# Аналог rg, но не следовать ссылкам
alias rgnf='command rg --hidden --no-follow --glob="!.git/"'

# Аналог rg, но не исключать .git и .gitignore
alias rgall='command rg -uu --follow'

# Искать во всех файлах относительно корня системы
# следовать ссылкам, искать скрытые файлы, не искать .git и .gitignore
# исключить заведомо ненужные директории
# Относительные пути в аргументах разрешаются от /; cwd вызывающей оболочки не меняется.
rgroot() (
    cd / || return
    command rg \
        -uu \
        --follow \
        --glob='!/proc/**' \
        --glob='!/sys/**' \
        --glob='!/dev/**' \
        --glob='!/run/**' \
        --glob='!/mnt/**' \
        --glob='!/nix/**' \
        --glob='!/var/run/**' \
        "$@" /
)
