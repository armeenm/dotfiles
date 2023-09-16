{ config, pkgs, lib, root, user, inputs, ... }:

{
  fonts.fontconfig.enable = lib.mkForce true;

  imports = [
    ./home.nix
  ];
}

#  home-manager.users."${user.login}" = {
#    _module.args = { sys = config; };
#    home = import ./home.nix args;
#    programs = import ./programs.nix args;
#    services = import ./services.nix args;
#    systemd = import ./systemd.nix args;
#    xdg = import ./xdg.nix args;
#    gtk = import ./gtk.nix args;
#    wayland = import ./wayland.nix args;
