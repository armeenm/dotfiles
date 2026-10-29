{ inputs
, lib
, pkgs
, modulesPath
, user
, ...
}:

# Settings for the full interactive hosts. The common base is in ./base.nix.
{
  imports = [
    ./base.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  age = {
    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  };

  boot = {
    kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;
  };

  time.timeZone = null;

  console = {
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
  };

  nix.settings = {
    allowed-users = lib.mkForce [ "@users" ];

    # Keep enough headroom for a rebuild to fetch a new system closure.
    min-free = 10 * 1024 * 1024 * 1024;
  };

  nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];

  services = {
    devmon.enable = true;
    fwupd.enable = true;
    pcscd.enable = true;
    smartd.enable = true;
    udisks2.enable = true;
    userborn.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
    };

    openssh.settings.X11Forwarding = true;

    tailscale = {
      enable = true;
    };
  };

  systemd = {
    oomd = {
      enable = true;
      enableRootSlice = true;
      enableSystemSlice = true;
      enableUserSlices = true;
    };

    suppressedSystemUnits = [
      "sys-kernel-debug.mount"
    ];

    settings = {
      Manager.RebootWatchdogSec = 15;
    };
  };

  security = {
    allowUserNamespaces = true;

    apparmor.enable = true;
    polkit.enable = true;

    # eMudhra intermediate CA compromise (October 2026). Nixpkgs' cacert
    # already distrusts this root, but keep it here explicitly so the
    # protection does not depend on the nixpkgs revision.
    # https://emudhra.we-have-a-problem.com/
    #
    # "emSign ECC Root CA - G3" is no longer listed: cacert 3.131.1 removed it
    # from the store entirely, and buildcatrust fails when a blacklisted cert
    # is missing.
    pki.caCertificateBlacklist = [
      "Comodo AAA Services root"
    ];
    rtkit.enable = true;
    sudo.enable = false;

    tpm2 = {
      enable = true;
      abrmd.enable = true;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };
  };

  users = {
    defaultUserShell = pkgs.zsh;

    users."${user.login}" = {
      # NOTE: Userborn doesn't use these yet, so these are useless currently.
      subGidRanges = [
        {
          count = 65536;
          startGid = 100000;
        }
      ];
      subUidRanges = [
        {
          count = 65536;
          startUid = 100000;
        }
      ];
    };
  };

  programs = {
    mosh.enable = true;
    mtr.enable = true;
    nix-ld.enable = true;
    zsh.enable = true;

    appimage = {
      enable = true;
      binfmt = true;
    };

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
    # Keep our zsh path ahead of the one programs.zsh adds.
    pathsToLink = lib.mkBefore [ "/share/zsh" ];

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
      htop
      rsync

      bcachefs-tools
      dosfstools
      exfatprogs
      f2fs-tools
      ntfs3g

      # (hunspellWithDicts [ hunspellDicts.en_US hunspellDicts.en_US-large ])
    ];

    etc = {
      subuid = {
        mode = "0644";
        text = ''
          ${user.login}:100000:65536
        '';
      };

      subgid = {
        mode = "0644";
        text = ''
          ${user.login}:100000:65536
        '';
      };
    };
  };

  documentation = {
    dev.enable = true;
    man.cache.enable = true;
  };

  system = {
    etc.overlay.enable = true;
    nixos-init.enable = true;

    activationScripts.report-changes = ''
        PATH=$PATH:${lib.makeBinPath [ pkgs.nvd pkgs.nix ]}
        nvd diff $(ls -dv /nix/var/nix/profiles/system-*-link | tail -2)
    '';
  };

  xdg.autostart.enable = lib.mkForce false;

  zramSwap.enable = true;
}
