{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.boot.custom.efi;
in {
  options.boot.custom.efi.enable = mkOption {
    default = false;
    type = types.bool;
    example = true;
  };

  config = mkIf cfg.enable {
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
