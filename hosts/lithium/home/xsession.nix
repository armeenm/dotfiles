{ config, pkgs, ... }:

{
  enable = true;

  pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ-AA";
    size = 16;
  };
}
