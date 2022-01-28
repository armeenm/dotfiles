# Basic ISO for installation
{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  networking = {
    wireless.enable = false;
    networkmanager.enable = true;
  };

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  programs = {
    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
    };
  };

  environment.systemPackages = with pkgs; [
    btop
    git
    htop
  ];
}
