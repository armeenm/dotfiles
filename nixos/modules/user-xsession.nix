{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.services.xserver.user-xsession;
in {
  options.services.xserver.user-xsession = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.desktopManager.session = [{
      name = "user-xsession";
      start = ''
        ${pkgs.runtimeShell} $HOME/.xsession &
        waitPID=$!
      '';
    }];
  };
}
