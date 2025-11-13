{ config, user, ... }:

{
  # No more support for x86 macOS.
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Assume Determinate Nix on macOS.
  nix.enable = false;
  determinate-nix.customSettings = {
    eval-cores = 0;
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
