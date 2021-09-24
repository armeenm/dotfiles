{ config, pkgs, ... }:

pkgs.linuxPackages_latest.nvidia_x11.overrideAttrs (old: {
  peepee = old.eeee;
  makeFlags = old.makeFlags ++ [ ''-DDEFAULT_RC_FILE="~/.config/nvidia-settings-rc"'' ];
})
