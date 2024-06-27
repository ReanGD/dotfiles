{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    # std
    meld              # git UI diff tool
    sublime-merge     # git UI tool
  ];
}
