{ config, pkgs, ... }:

{
  imports = [
    ./pkg_fd.nix      # fast find alternative
    ./pkg_ripgrep.nix # fast grep alternative
    ./pkg_zsh.nix     # modern ls alternative (old exa)
    ./pkg_eza.nix     # shell
  ];

  home.packages = with pkgs; [
    ncdu     # disk usage analyzer
    fzf      # fuzzy search
  ];
}
