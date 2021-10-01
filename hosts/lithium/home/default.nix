{ config, pkgs, lib, root, user, ... }:

let
  args = {
    inherit pkgs lib root user;
    sys = config;
    config = config.home-manager.users."${user.login}";
    misc = {
      restart-wm = "${pkgs.xmonad-with-packages}/bin/xmonad --restart";
    };
  };
in
{
  home-manager.users."${user.login}" = {
    home = import ./home.nix args;
    xdg = import ./xdg.nix args;
    xsession = import ./xsession.nix args;
    xresources = import ./xresources.nix args;
    services = import ./services.nix args;
    programs = import ./programs.nix args;
    systemd = import ./systemd.nix args;

    fonts.fontconfig.enable = lib.mkForce true;
  };
}
