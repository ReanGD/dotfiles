{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    cherry-studio  # Desktop client that supports for multiple LLM providers
    uv             # dependency for cherry-studio
    bun            # dependency for cherry-studio
  ];

  home.file = {
    ".cherrystudio/bin/uv".source = "${pkgs.uv}/bin/uv";
    ".cherrystudio/bin/bun".source = "${pkgs.bun}/bin/bun";
  };
}
