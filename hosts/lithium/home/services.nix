{ config, pkgs, lib, root, user, misc, ... }:

{
  flameshot.enable = true;
  playerctld.enable = true;

  emacs = {
    enable = true;
    client.enable = true;
    client.arguments = [ "-c" "-n" ];
  };

  dunst = {
    enable = true;
    configFile = "${root}/conf/dunst/dunstrc";
  };

  screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = with config.home; "${homeDirectory}/${file.lock.target}";
  };

  grobi = {
    enable = false;
    executeAfter = [ misc.restart-wm ];
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
