{ config, pkgs, ... }:

{
  enable = false;

  cursorTheme = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ-AA";
    size = 16;
  };
}
