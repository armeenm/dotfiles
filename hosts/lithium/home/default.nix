{ config, pkgs, lib, root, user, domain, inputs, ... }:

let
  args = {
    inherit pkgs lib root user domain inputs;
    sys = config;
    config = config.home-manager.users."${user.login}";
  };
in
{
  #imports = [ pkgs.nur.repos.rycee.hmModules.emacs-init ];

  home-manager.users."${user.login}" = {
    home = import ./home.nix args;
    programs = import ./programs.nix args;
    services = import ./services.nix args;
    systemd = import ./systemd.nix args;
    xdg = import ./xdg.nix args;
    gtk = import ./gtk.nix args;

    fonts.fontconfig.enable = lib.mkForce true;
  };
}
