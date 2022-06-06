inputs@{ config, pkgs, lib, user, domain, ... }:

let
  hostName = "francium";
  domain = inputs.domain;

  root = "";
  nic = "";
in
{
  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      device = root;
    };
  };

  fileSystems = {
    "/" = {
      device = root;
      fsType = "ext4";
    };
  };

  nix.trustedUsers = [ "@wheel" ];

  networking = {
    inherit hostName domain;
    interfaces.${nic}.useDHCP = true;

    firewall = {
      allowedTCPPorts = [ 22 ];
    };
  };

  time.timeZone = "America/Chicago";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = true;
  };

  users = {
    defaultUserShell = pkgs.zsh;

    users = {
      root = {
        home = lib.mkForce "/home/root";
      };

      "${user.login}" = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
      };
    };
  };

  environment = {
    defaultPackages = lib.mkForce [ ];
    systemPackages = with pkgs; [
      bottom
      fd
      git
      hdparm
      ldns
      lm_sensors
      lshw
      nmap
      pciutils
      profanity
      ripgrep
      rsync
      tmux
      tree
      usbutils
      wget
    ];

    shellAliases = {
      sudo = "doas";
    };
  };

  programs = {
    mosh.enable = true;
    mtr.enable = true;
    zsh.enable = true;

    neovim = {
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
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

  services = {
    fstrim.enable = true;
    haveged.enable = true;
    openssh.enable = true;
    smartd.enable = true;
    timesyncd.enable = true;
    udisks2.enable = true;
  };

  security = {
    auditd.enable = true;
    sudo.enable = false;

    allowUserNamespaces = true;
    protectKernelImage = true;
    unprivilegedUsernsClone = false;

    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
        keepEnv = true;
      }];
    };
  };

  system.stateVersion = lib.mkForce "22.05";
}
