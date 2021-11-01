{ config, nixos-hardware, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.custom.nvidia;
in {
  options.hardware.custom.nvidia = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.videoDrivers = [ "nvidia" ];
    
    hardware.nvidia = {
      package = config.boot.kernelPackages.nvidia_x11_beta;
      modesetting.enable = true;
    };
  };
}
