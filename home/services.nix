{ config, pkgs, lib, sys, root, user, ... }:

{
  mpd-mpris.enable = true;
  mpris-proxy.enable = true;
  playerctld.enable = true;

  emacs = {
    enable = true;
    # TODO: Fix upstream.
    defaultEditor = false;

    client = {
      enable = true;
      arguments = [ "-n" "-t" "-c" ];
    };
  };

  mako = {
    enable = true;
    extraConfig = ''
      [mode=do-not-disturb]
      invisible=1
    '';
  };

  mpd = {
    enable = true;
    network.startWhenNeeded = true;
  };
}
