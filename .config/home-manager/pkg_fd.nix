{ config, lib, pkgs, ... }:
{
  # fast find alternative
  programs.fd = {
    enable = true;
    hidden = true;
    ignores = [
      ".git/"
      "/mnt"
      "/proc"
    ];
    extraOptions = [
      "--follow"
    ];
  };
}
