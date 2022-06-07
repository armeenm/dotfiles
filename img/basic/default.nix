# Basic ISO for installation
{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  networking = {
    wireless.enable = false;
    networkmanager.enable = true;
  };

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = false;
  };

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      warn-dirty = false
      experimental-features = nix-command flakes ca-derivations
    '';
  };

  programs = {
    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      defaultEditor = true;
      configure = {
        customRC = ''
          set number
          set hidden
          set shell=bash
          set cmdheight=2
          set nocompatible
          set shortmess+=c
          set updatetime=300
          set background=dark
          set foldmethod=marker
          set signcolumn=yes
          set nobackup nowritebackup
          set tabstop=2 shiftwidth=2 expandtab
          set tagrelative
          set tags^=./.git/tags;
          set mouse=a
        '';
      };
    };
  };

  environment.systemPackages = with pkgs; [
    btop
    gh
    git
    hdparm
    htop
    lm_sensors
    lshw
    pciutils
    smartmontools
    usbutils
  ];
}
