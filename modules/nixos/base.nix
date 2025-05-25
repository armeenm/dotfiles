{ config, inputs, lib, pkgs, modulesPath, user, root, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  age = {
    secrets = {
      "${user.login}-pw".file = "${root}/secrets/${user.login}-pw.age";
      "arash-pw".file = "${root}/secrets/arash-pw.age";
    };

    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  };

  boot.initrd.verbose = false;

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = null;

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = false;
  };

  services = {
    devmon.enable = true;
    fstrim.enable = true;
    fwupd.enable = true;
    pcscd.enable = true;
    rpcbind.enable = true;
    smartd.enable = true;
    timesyncd.enable = true;
    udisks2.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
    };

    openssh = {
      enable = true;

      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = true;
      };
    };
  };

  systemd = {
    oomd = {
      enable = true;
      enableRootSlice = true;
      enableSystemSlice = true;
      enableUserSlices = true;
    };

    watchdog.rebootTime = "15s";

    suppressedSystemUnits = [
      "sys-kernel-debug.mount"
    ];
  };

  security = {
    allowUserNamespaces = true;
    protectKernelImage = true;
    unprivilegedUsernsClone = true;
    virtualisation.flushL1DataCache = null;

    apparmor.enable = true;
    auditd.enable = true;
    rtkit.enable = true;
    polkit.enable = true;
    sudo.enable = false;

    audit = {
      enable = false;
      rules = [ ];
    };

    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
        keepEnv = true;
        noPass = false;
        persist = true;
      }];
    };

    tpm2 = {
      enable = true;
      abrmd.enable = true;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;

    users = {
      root.hashedPassword = null;

      arash = {
        isNormalUser = true;
        hashedPasswordFile = config.age.secrets.arash-pw.path;
      };

      "${user.login}" = {
        isNormalUser = true;
        hashedPasswordFile = config.age.secrets."${user.login}-pw".path;
        extraGroups = [
          "wheel"
        ];
      };
    };
  };

  programs = {
    mosh.enable = true;
    mtr.enable = true;
    nix-ld.enable = true;
    zsh.enable = true;

    neovim = {
      enable = true;
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

  environment = {
    defaultPackages = lib.mkForce [ ];

    pathsToLink = [ "/share/zsh" ];

    systemPackages = with pkgs; [
      doas-sudo-shim
      ethtool
      hdparm
      keyutils
      lm_sensors
      lshw
      nfs-utils
      pciutils
      sbctl
      smartmontools
      usbutils

      git
      rsync

      (hunspellWithDicts [ hunspellDicts.en_US hunspellDicts.en_US-large ])
    ];
  };

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

  system = {
    activationScripts.report-changes = ''
        PATH=$PATH:${lib.makeBinPath [ pkgs.nvd pkgs.nix ]}
        nvd diff $(ls -dv /nix/var/nix/profiles/system-*-link | tail -2)
    '';
  };

  zramSwap.enable = true;
}
