args@{ pkgs, lib, inputs, root, user, ... }:

{
  nixpkgs = {
    hostPlatform = "aarch64-darwin";
    config.allowUnfree = true;
  };

  users.users.${user.login} = {
    name = "armeen";
    home = "/Users/armeen";
  };

  home-manager = {
    users."${user.login}" = import "${root}/home";
    extraSpecialArgs = {
      inherit inputs root user;
      stateVersion = "24.11";
    };
  };
}
