{ config, pkgs, lib, root, user, misc, ... }:

{
  playerctld.enable = true;

  emacs = {
    enable = true;
    client.enable = true;
    client.arguments = [ "-c" "-n" ];
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
      "CBB258EB7AED9AED74E0F9126E369691F40D3371"
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
