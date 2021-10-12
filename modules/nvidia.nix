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

  imports = [ ./nvidia-module.nix ];
  disabledModules = [ "hardware/video/nvidia.nix" ];

  config = mkIf cfg.enable {
    services.xserver.videoDrivers = [ "nvidia" ];
    
   # boot.initrd.kernelModules = [
   #   "nvidia"
   #   "nvidia_modeset"
   #   "nvidia_uvm"
   #   "nvidia_drm"
   # ];

    hardware.nvidia = {
      modesetting.enable = true;
      powerManagement.enable = true;
    };
  };
}
