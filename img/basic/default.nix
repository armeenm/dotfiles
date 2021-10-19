# Basic ISO for installation
{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;

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

    bash.shellInit = builtins.readFile ./crypt.sh;
  };

  environment.systemPackages = with pkgs; [
    git
    sudo
    bottom
    networkmanager
    neovim
    gcc
    openssl
    yubikey-personalization
  ];
}
