{ config, lib, user, ... }:

{
  # No more support for x86 macOS.
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Assume Determinate Nix on macOS.
  determinateNix = {
    enable = true;
    distributedBuilds = true;
    registry = config.nix.registry;

    determinateNixd = {
      builder.state = "enabled";
    };

    customSettings = config.nix.settings // {
      allowed-users = lib.mkForce [ "@staff" ];
      trusted-users = lib.mkForce [ "@admin" ];
    };
  };

  power.restartAfterFreeze = true;

  services = {
    emacs = {
      enable = true;
      package = config.home-manager.users.${user.login}.programs.emacs.package;
    };
  };

  users.users.${user.login} = {
    home = "/Users/${user.login}";
  };
}
