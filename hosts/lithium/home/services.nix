{ config, pkgs, lib, root, user, ... }:

{
  mpris-proxy.enable = true;
  playerctld.enable = true;

  emacs = {
    enable = true;
    defaultEditor = true;

    client = {
      enable = true;
      arguments = [ "-c" "-n" ];
    };
  };

  screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = with config.home; "${homeDirectory}/${file.lock.target}";
  };

  gammastep = {
    enable = false;
    latitude = 40.1019564;
    longitude = -88.2293502;
  };

  gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "curses";
    sshKeys = [
      "04D42E929F2A312225856CD740A092BEE315D631"
    ];
    extraConfig = ''
      homedir ${config.programs.gpg.homedir}
      allow-loopback-pinentry
    '';
  };

  mpd = {
    enable = true;
    network.startWhenNeeded = true;
  };
}
