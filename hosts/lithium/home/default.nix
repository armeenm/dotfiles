{ config, pkgs, lib, root, user, domain, inputs, ... }:

let
  args = {
    inherit pkgs lib root user domain inputs;
    sys = config;
    config = config.home-manager.users."${user.login}";
  };
in
{
  home-manager.users."${user.login}" = {
    home = import ./home.nix args;
    programs = import ./programs.nix args;
    services = import ./services.nix args;
    systemd = import ./systemd.nix args;
    xdg = import ./xdg.nix args;
    xsession = import ./xsession.nix args;

    fonts.fontconfig.enable = lib.mkForce true;
  };
}
