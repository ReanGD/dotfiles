function git_change_user() {
	if [[ "$(command git config user.name)" == "ReanGD" ]]; then
        cp $XDG_CONFIG_HOME/git/config.work $XDG_CONFIG_HOME/git/config
    else
        cp $XDG_CONFIG_HOME/git/config.home $XDG_CONFIG_HOME/git/config
    fi
}
