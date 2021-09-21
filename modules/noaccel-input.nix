{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.services.xserver.custom.noAccelInput;
in {
  options.services.xserver.custom.noAccelInput = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.libinput = {
      enable = true;
      mouse.accelProfile = "flat";
      mouse.accelSpeed = "0";
      touchpad.accelProfile = "flat";
      touchpad.accelSpeed = "0";
    };
  };
}
