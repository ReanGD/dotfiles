{ config, lib, pkgs, ... }:
{
  programs.atuin = {
    enable = true;
    enableZshIntegration = false;
    settings = {
      auto_sync = false;
      update_check = false;
      # search_mode = "prefix";
    };
  };

  programs.zsh = {
    enable = true;
    shellAliases = {
      # see https://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo
      sudo="sudo ";
      sudoe="sudo --preserve-env ";
    };
    dotDir = "${config.xdg.configHome}/zsh";
    initContent = lib.mkOrder 550 ''
      ZSH_DATA_DIR="${config.xdg.dataHome}/zsh"
      ZSH_CACHE_DIR="${config.xdg.cacheHome}/zsh"
      ZSH_CONFIG_DIR="${config.xdg.configHome}/zsh"
      ZSH_LIB_DIR=$ZSH_CONFIG_DIR/lib

      # For agkozak/zsh-z
      ZSHZ_DATA="$XDG_DATA_HOME/.z"
      # For zsh-users/zsh-autosuggestions
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10"
      # For compinit
      ZSH_COMPDUMP="$ZSH_DATA_DIR/.zcompdump-$HOST-$ZSH_VERSION"
      # For atuin
      ATUIN_NOBIND="true"

      source $ZSH_LIB_DIR/common.zsh
      source $ZSH_LIB_DIR/options_nix.zsh
      source $ZSH_LIB_DIR/history.zsh
      source $ZSH_LIB_DIR/aliases_nix.zsh
      source $ZSH_LIB_DIR/correction.zsh
      source $ZSH_LIB_DIR/prompt.zsh
      source $ZSH_LIB_DIR/completion.zsh

      if [[ $options[zle] = on ]]; then
        eval "$(${pkgs.atuin}/bin/atuin init zsh )"
      fi

      source $ZSH_LIB_DIR/key_bindings.zsh
    '';
    completionInit = "source $ZSH_LIB_DIR/compinit.zsh";
    antidote = {
      enable = true;
      plugins = [
        # fast cd to a directory
        "agkozak/zsh-z"
        # set title of terminal tab
        "trystan2k/zsh-tab-title"
        # fish-like autosuggestions
        "zsh-users/zsh-autosuggestions"
        "zsh-users/zsh-completions"
        "zdharma-continuum/fast-syntax-highlighting"
      ];
      # need test
      # Aloxaf/fzf-tab
      # alternative
      # mattmc3/zman = my fman
    };
    # setopt AUTO_CD\autocd - goto directory without "cd"
    autocd = true;
    envExtra = ''
      fpath=(${config.xdg.configHome}/zsh/lib/funcs $fpath)
    '';
    history = {
      size = 20000; # HISTSIZE
      save = 20000; # SAVEHIST
      share = false; # SHARE_HISTORY
      extended = true; # EXTENDED_HISTORY
      expireDuplicatesFirst = true; # HIST_EXPIRE_DUPS_FIRST
      ignoreDups = false; # HIST_IGNORE_DUPS
      ignoreAllDups = true; # HIST_IGNORE_ALL_DUPS
      saveNoDups = true; # HIST_SAVE_NO_DUPS
      path = "$ZSH_DATA_DIR/zsh_history"; # HISTFILE
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
