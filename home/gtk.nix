{ config, isHeadless, pkgs, ... }:

{
  gtk = {
    enable = !isHeadless;
    cursorTheme = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ-AA";
      size = 16;
    };
  };
}
