{ lib, user, pkgs, ... }:

{
  home-manager = {
    extraSpecialArgs = {
      enableSocial = true;
      isPortable = true;
    };

    users.${user.login}.home.stateVersion = lib.mkForce "26.05";
  };

  system.stateVersion = 6;
}
