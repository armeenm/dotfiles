{ config, lib, pkgs, ... }:

{
  nix.extraOptions = ''
    experimental-features = nix-command flakes ca-derivations
  '';

  environment.packages = with pkgs; [ neovim ];

  system.stateVersion = "24.05";
}
