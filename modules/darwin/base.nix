{ config, inputs, lib, user, ... }:

{
  # No more support for x86 macOS.
  nixpkgs = {
    hostPlatform = "aarch64-darwin";
    overlays = [
      # Provides `emacs-unstable` and refreshed ELPA/MELPA package sets.
      inputs.emacs-overlay.overlays.default
      # Must come last: this `emacsWithPackagesFromUsePackage` is the one that
      # understands `configAsDefaultInitFile`, which the shared home config
      # passes on Darwin.
      inputs.darwin-emacs.overlays.emacs
    ];
  };

  # Assume Determinate Nix on macOS.
  determinateNix = {
    enable = true;
    distributedBuilds = true;
    registry = config.nix.registry;

    determinateNixd = {
      builder.state = "enabled";
    };

    customSettings = config.nix.settings // {
      sandbox = "relaxed";
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
