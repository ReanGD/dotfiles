{ config, lib, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    shellAliases = {
      # see https://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo
      sudo="sudo ";
      sudoe="sudo --preserve-env ";
    };
    dotDir = ".config/zsh";
    initExtraBeforeCompInit = ''
      ZSH_DATA_DIR="${config.xdg.dataHome}/zsh"
      ZSH_CACHE_DIR="${config.xdg.cacheHome}/zsh"
      ZSH_CONFIG_DIR="${config.xdg.configHome}/zsh"
      ZSH_LIB_DIR=$ZSH_CONFIG_DIR/lib

      # For ohmyzsh/ohmyzsh path:plugins/colorize
      ZSH_COLORIZE_STYLE="monokai"
      # For agkozak/zsh-z
      ZSHZ_DATA="$XDG_DATA_HOME/.z"
      # For ohmyzsh/ohmyzsh path:plugins/alias-finder
      ZSH_ALIAS_FINDER_AUTOMATIC=true
      # For zsh-users/zsh-autosuggestions
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10"
      # For compinit
      ZSH_COMPDUMP="$ZSH_DATA_DIR/.zcompdump-$HOST-$ZSH_VERSION"

      source $ZSH_LIB_DIR/common.zsh
      source $ZSH_LIB_DIR/options.zsh
      source $ZSH_LIB_DIR/history.zsh
      source $ZSH_LIB_DIR/aliases.zsh
      source $ZSH_LIB_DIR/correction.zsh
      source $ZSH_LIB_DIR/prompt.zsh
      source $ZSH_LIB_DIR/key_bindings.zsh
      source $ZSH_LIB_DIR/completion.zsh
    '';
    completionInit = "source $ZSH_LIB_DIR/compinit.zsh";
    antidote = {
      enable = true;
      plugins = [
        # deps for ohmyzsh plugins
        "ohmyzsh/ohmyzsh path:lib/clipboard.zsh"
        # ctrl-o - copy current text in the command line to the system clipboard
        "ohmyzsh/ohmyzsh path:plugins/copybuffer"
        # copyfile <filename> to copy the file named filename
        "ohmyzsh/ohmyzsh path:plugins/copyfile"
        # copypath: copies the absolute path of the current directory.
        # copypath <file_or_directory>: copies the absolute path of the given file.
        "ohmyzsh/ohmyzsh path:plugins/copypath"
        # extract "x ./tmp.zip"
        "ohmyzsh/ohmyzsh path:plugins/extract"
        # learning new aliases
        "ohmyzsh/ohmyzsh path:plugins/alias-finder"
        # ccat\cless <file> - colorize file content
        "ohmyzsh/ohmyzsh path:plugins/colorize"
        # fast cd to a directory
        "agkozak/zsh-z"
        "supercrabtree/k"
        # echo Hello world | pbcopy
        # echo Hello world | clip
        # pbpaste | grep Hello
        # clip | grep Hello
        "zpm-zsh/clipboard"
        "trystan2k/zsh-tab-title"
        "zsh-users/zsh-autosuggestions"
        "zsh-users/zsh-completions"
        "zdharma-continuum/fast-syntax-highlighting"
      ];
      # need test
      # Aloxaf/fzf-tab
      # OMZP::docker
      # OMZP::compleat = mbrubeck/compleat

      # alternative
      # mattmc3/zman = my fman
      # OMZP::extract = thetic/extract
      # OMZP::alias-finder = sei40kr/zsh-fast-alias-tips = djui/alias-tips
    };
    history = {
      # HISTSIZE
      size = 20000;
      # SAVEHIST
      save = 20000;
      # unsetopt SHARE_HISTORY
      share = false;
      # setopt EXTENDED_HISTORY
      extended = true;
      # setopt HIST_EXPIRE_DUPS_FIRST
      expireDuplicatesFirst = true;
      # setopt HIST_IGNORE_DUPS
      ignoreDups = true;
      # setopt HIST_IGNORE_ALL_DUPS
      ignoreAllDups = true;
      # setopt HIST_IGNORE_SPACE
      ignoreSpace = true;
      # HISTFILE
      path = "$ZSH_DATA_DIR/zsh_history";
      # HISTORY_IGNORE
      ignorePatterns = [
        "ls"
        "ll"
        "la"
        "pwd"
        "history"
        "exit"
        "clear"
        "cd"
      ];
    };
  };

  home.activation.zshBefore = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    rm -f $XDG_CONFIG_HOME/zsh/.zshenv
  '';

  home.activation.zshAfter = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    rm -f $HOME/.zshenv

    zshenvPath="$XDG_CONFIG_HOME/zsh/.zshenv"
    if [ -f "$zshenvPath" ]; then
      sed -i 's|\.nix-profile/|\.local/state/nix/profile/|g' "$zshenvPath"
    fi
  '';
}
