{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.boot.custom.luks-yubikey;
in {
  options.boot.custom.luks-yubikey = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };

    boot = mkOption {
      default = null;
      type = types.nullOr types.path;
    };

    root = mkOption {
      default = null;
      type = types.nullOr types.path;
    };
  };

  config = mkIf cfg.enable {
    boot.initrd = {
      kernelModules = [
        "vfat"
        "nls_cp437"
        "nls_iso8859-1"
        "usbhid"
      ];

      luks = {
        yubikeySupport = true;

        devices = {
          "nixos-enc" = {
            device = cfg.root;
            preLVM = true;
            allowDiscards = true;

            yubikey = {
              slot = 2;
              twoFactor = true;
              storage.device = cfg.boot;
            };
          };
        };
      };
    };
  };
}
