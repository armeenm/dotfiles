{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.xserver.autoLoginUser;
in {
  options.xserver.autoLoginUser = {
    enable = mkOption {
      default = null;
      type = types.nullOr types.str;
    };
  };

  config = mkIf (cfg.enable != null) {
    services.xserver.displayManager = {
      lightdm.greeter.enable = false;
      autoLogin.user = cfg;
    };
  };
}
