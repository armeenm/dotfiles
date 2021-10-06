{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };

  environment.systemPackages = with pkgs; [
    git
    sudo
    bottom
    networkmanager
    neovim
  ];
}
