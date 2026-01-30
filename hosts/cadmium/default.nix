{ lib, user, ... }:

{
  home-manager = {
    extraSpecialArgs = {
      enableSocial = true;
      isPortable = true;
    };

    users.${user.login}.home.stateVersion = lib.mkForce "25.11";
  };

  system.stateVersion = 6;
}
