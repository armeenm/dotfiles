{ config, pkgs, lib, inputs, root, user, ... }:

{
  nixpkgs = {
    hostPlatform = "aarch64-darwin";
    config.allowUnfree = true;
  };

  nix = {
    package = pkgs.nixVersions.latest;
    channel.enable = true;
    nixPath = lib.mkForce [ "nixpkgs=${inputs.nixpkgs}" ];

    settings = {
      allowed-users = lib.mkForce [ "@everyone" ];
      trusted-users = lib.mkForce [ "@admin" ];

      experimental-features = [
        "ca-derivations"
        "flakes"
        "nix-command"
        "recursive-nix"
      ];

      warn-dirty = false;
    };
  };

  users.users.${user.login} = {
    name = "armeen";
    home = "/Users/armeen";
  };

  home-manager = {
    users."${user.login}" = import "${root}/home";

    extraSpecialArgs = {
      inherit pkgs inputs root user;
      stateVersion = "24.11";
    };
  };

  environment = {
    systemPackages = with pkgs; [
      git
    ];
  };

  programs = {
    zsh.enable = true;
    vim = {
      enable = true;
      enableSensible = true;
      vimConfig = ''
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

  services = {
    nix-daemon.enable = true;
    emacs = {
      enable = true;
      package = config.home-manager.users.${user.login}.programs.emacs.package;
    };
  };

  system = {
    stateVersion = 5;

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };
    defaults = {
      NSGlobalDomain = {
        AppleInterfaceStyleSwitchesAutomatically = true;
        AppleKeyboardUIMode = 3;
        InitialKeyRepeat = 10;
        KeyRepeat = 1;
      };

      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;

      alf = {
        globalstate = 1;
        stealthenabled = 1;
      };

      dock = {
        autohide = true;
        wvous-bl-corner = 3; # Application Windows
        wvous-br-corner = 4; # Desktop
        wvous-tr-corner = 7; # Dashboard
      };

      screencapture = {
        location = "~/Pictures/Screenshots";
        type = "png";
      };

      trackpad = {
        Clicking = true;
      };

      finder = {
        ShowPathbar = true;
        ShowStatusBar = true;
        FXPreferredViewStyle = "Nlsv"; # List view
        _FXShowPosixPathInTitle = false;
        AppleShowAllExtensions = true;
      };
    };
  };
}
