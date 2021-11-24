{ config, pkgs, lib, root, user, misc, ... }:

{
  flameshot.enable = true;
  playerctld.enable = true;

  emacs = {
    enable = false;
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
    pinentryFlavor = "emacs";
    extraConfig = ''
      homedir ${config.programs.gpg.homedir}
    '';
  };
  
  mpd = {
    enable = true;
    network.startWhenNeeded = true;
  };
}
