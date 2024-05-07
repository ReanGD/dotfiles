{ config, lib, pkgs, ... }:
{
  # fast grep alternative
  programs.ripgrep = {
    enable = true;
    arguments = [
      "--hidden"
      "--follow"
      "--glob=!.git/*"
      "--glob=!/mnt/*"
      "--glob=!/proc/*"
    ];
  };
}
