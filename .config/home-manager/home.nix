{ config, lib, pkgs, this, ... }:
with lib;
let
in
{
  home.username = "rean";
  home.homeDirectory = "/home/rean";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # support XDG Shared MIME-info specification and XDG MIME Applications
  xdg.mime.enable = true;
  # Home Manager work better on GNU/Linux distributions other than NixOS.
  # Support xdg and etc.
  targets.genericLinux.enable = true;

  imports = [
    ./group_system.nix
    ./group_development.nix
  ];

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # pkgs.htop    # process monitoring
    # It is sometimes useful to fine-tune packages, for example, by applying
    # overrides. You can do that directly here, just don't forget the
    # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # You can also create simple shell scripts directly inside your
    # configuration. For example, this adds a command 'my-hello' to your
    # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # environment.etc."myTextFile" = {
  #   source = pkgs.writeTextFile {
  #     name = "myTextFile";
  #     text = ''
  #       Это мой текст, который будет записан в файл.
  #       Вы можете добавить сюда любой текст, который хотите.
  #     '';
  #   };
  #   mode = "0644"; # Устанавливает права доступа к файлу
  # };


  systemd.user.startServices=true;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
    # "/etc/keyd/keys1.conf".text = ''
    #   [main]
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/rean/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
    # TMP1 = "nix";
  };
}
