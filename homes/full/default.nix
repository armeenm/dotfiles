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
    _module.args = { sys = config; };
    home = import ./home.nix args;
    programs = import ./programs.nix args;
    services = import ./services.nix args;
    systemd = import ./systemd.nix args;
    xdg = import ./xdg.nix args;
    gtk = import ./gtk.nix args;

    imports = [ config.nur.repos.rycee.hmModules.emacs-init ];

    fonts.fontconfig.enable = lib.mkForce true;
  };
}
