{ config, pkgs, lib, root, user, ... }:

{
  playerctld.enable = true;

  emacs = {
    enable = true;
    package = config.programs.emacs.package;

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
    pinentryFlavor = "gnome3";
    sshKeys = [
      "0C855A22D96790A9B1BEFFD4F4BDB31E141F67FF"
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
