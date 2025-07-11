{ config
, inputs
, lib
, pkgs
, modulesPath
, user
, ...
}:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  age = {
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

  home-manager = {
    users."${user.login}" = import ../../home;

    extraSpecialArgs = {
      inherit inputs user;
      stateVersion = config.system.stateVersion;
      isHeadless = true;
      isStandalone = false;
      isPortable = false;
      enableSocial = false;
    };
  };

  services = {
    chrony.enable = true;
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
        PermitRootLogin = "prohibit-password";
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

    services.systemd-timesyncd.enable = false;
  };

  security = {
    allowUserNamespaces = true;
    protectKernelImage = true;
    unprivilegedUsernsClone = true;
    virtualisation.flushL1DataCache = null;

    apparmor.enable = true;
    auditd.enable = true;
    polkit.enable = true;
    rtkit.enable = true;
    sudo.enable = false;
    sudo-rs.enable = true;

    audit = {
      enable = false;
      rules = [ ];
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

      "${user.login}" = {
        isNormalUser = true;
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
          set background=dark
          set cmdheight=2
          set foldmethod=marker
          set hidden
          set mouse=a
          set nobackup nowritebackup
          set nocompatible
          set number
          set shell=bash
          set shortmess+=c
          set signcolumn=yes
          set tabstop=2 shiftwidth=2 expandtab
          set tagrelative
          set tags^=./.git/tags;
          set updatetime=300
        '';
      };
    };
  };

  environment = {
    defaultPackages = lib.mkForce [ ];

    pathsToLink = [ "/share/zsh" ];

    systemPackages = with pkgs; [
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

      bcachefs-tools
      dosfstools
      exfatprogs
      f2fs-tools
      ntfs3g

      #(hunspellWithDicts [ hunspellDicts.en_US hunspellDicts.en_US-large ])
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

  xdg.autostart.enable = lib.mkForce false;

  zramSwap.enable = true;
}
