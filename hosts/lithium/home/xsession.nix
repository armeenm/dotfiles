{ config, pkgs, lib, root, user, ... }:

{
  enable = true;

  scriptPath = ".config/xsession";
  profilePath = ".config/xprofile";

  profileExtra = "autorandr --load default";

  pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ-AA";
    size = 16;
  };

  windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = "${root}/conf/xmonad/xmonad.hs";
  };
}
