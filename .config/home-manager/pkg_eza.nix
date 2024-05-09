{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    eza      # modern ls alternative (old exa)
  ];

  programs.zsh.shellAliases = {
    ls = "eza --grid";
    ll = "eza --long --group --header --links --mounts --all --git --git-repos --group-directories-first";
    lt = "eza --long --tree --links --mounts --no-time --no-user --no-permissions --no-git";
  };
}
