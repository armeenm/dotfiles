{ lib, user, ... }:

{
  home-manager.users.${user.login}.home.stateVersion = lib.mkForce "25.11";
  system.stateVersion = 6;
}
