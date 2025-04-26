{ config, pkgs, ... }:

{
  imports = [
    ./pkg_fd.nix            # fast find alternative
    ./pkg_ripgrep.nix       # fast grep alternative
    ./pkg_zsh.nix           # modern ls alternative (old exa)
    ./pkg_eza.nix           # shell
    ./pkg_cherry_studio.nix # llm desktop client
  ];

  home.packages = with pkgs; [
    # terminal
    ncdu     # disk usage analyzer
    fzf      # fuzzy search
    # archivers
    p7zip
    unrar
  ];
}
