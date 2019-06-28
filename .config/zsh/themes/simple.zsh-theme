# Directory info.
local current_dir='${PWD/#$HOME/~}'

# Git info.
# local git_info='$(git_prompt_info)'
# ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[white]%}on%{$reset_color%} git:%{$fg[cyan]%}"
local git_info='$(git_prompt_info_with_location)'
ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[white]%}on%{$reset_color%} git:"
ZSH_THEME_GIT_PROMPT_HOME="%{$fg[green]%}home%{$fg[cyan]%} "
ZSH_THEME_GIT_PROMPT_WORK="%{$fg[magenta]%}work%{$fg[cyan]%} "
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}x"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg[green]%}o"

# Prompt format: \n # USER at MACHINE in DIRECTORY on git:BRANCH STATE [TIME] \n $ 
PROMPT="
%{$terminfo[bold]$fg[blue]%}#%{$reset_color%} \
%{$terminfo[bold]$fg[yellow]%}${current_dir}%{$reset_color%}\
${git_info}
%{$terminfo[bold]$fg[red]%}$ %{$reset_color%}"