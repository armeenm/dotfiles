{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.services.xserver.autoLoginUser;
in {
  options.services.xserver.autoLoginUser = mkOption {
    default = null;
    type = types.nullOr types.str;
  };

  config = mkIf (cfg != null) {
    services.xserver.displayManager = {
      lightdm.greeter.enable = false;
      autoLogin.user = cfg;
    };
  };
}
