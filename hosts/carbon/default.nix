{ config, pkgs, lib, modulesPath, inputs, root, user, domain, ... }:

{
  imports = [
    ./router.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/E777-4036";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-uuid/580bb1f1-73ac-444a-b4ca-5e36b502cdb4";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  boot = {
    initrd.availableKernelModules = [
      "ahci"
      "xhci_pci"
      "ehci_pci"
      "usbhid"
      "usb_storage"
      "uas"
      "sd_mod"
      "sr_mod"
    ];

    kernelModules = [ "kvm-intel" ];

    supportedFilesystems = [ "ntfs" ];
    #binfmt.emulatedSystems = [ "aarch64-linux" ];

    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        editor = false;
      };
    };
  };

  hardware = {
    enableAllFirmware = true;

    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = true;
  };

  nix = {
    package = pkgs.nixUnstable;
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
    };

    settings = {
      allowed-users = lib.mkForce [ "@wheel" ];
      trusted-users = lib.mkForce [ "@wheel" ];

      experimental-features = [
        "flakes" "nix-command" "ca-derivations" "impure-derivations"
      ];

      warn-dirty = false;
    };
  };

  nixpkgs = {
    hostPlatform = "x86_64-linux";
  };

  services = {
    fstrim.enable = true;
    fwupd.enable = true;
    haveged.enable = true;
    smartd.enable = true;
    tcsd.enable = false;
    timesyncd.enable = true;
    udisks2.enable = true;

    hardware = {
      bolt.enable = true;
    };

    openssh = {
      enable = true;

      settings = {
        PasswordAuthentication = true;
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    vaultwarden = {
      enable = true;
    };

    nginx = {
      enable = false;
      virtualHosts."vault.armeen.xyz" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1$:8000";
      };
    };

  };

  systemd = {
    watchdog.rebootTime = "15s";

    tmpfiles.rules = [
      "d /run/cache 0755 - - -"
      "d /var/etc 0755 - - -"
      "d /var/srv 0755 - - -"
      "d /run/tmp 1777 - - -"

      "L /srv - - - - /var/srv"

      # Using /home/root instead
      "R /root - - - - -"
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
    sudo.enable = false;

    acme = {
      acceptTerms = true;
      defaults.email = user.email;
    };

    audit = {
      enable = false;
      rules = [ ];
    };

    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
        keepEnv = true;
        noPass = true;
      }];
    };

    pam = {
      u2f.enable = true;
    };

    tpm2 = {
      enable = true;
      abrmd.enable = false;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };
  };

  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = true; # XXX

    users = {
      root = {
        #hashedPassword = null;
        home = lib.mkForce "/home/root";
      };

      "${user.login}" = {
        isNormalUser = true;
        #passwordFile = config.sops.secrets."${user.login}-pw".path;
        extraGroups = [ "wheel" ];
      };
    };
  };

  environment = {
    defaultPackages = lib.mkForce [ ];

    systemPackages = with pkgs; [
      git
      hdparm
      lm_sensors
      lshw
      pciutils
      rsync
      smartmontools
      usbutils

      (hunspellWithDicts [ hunspellDicts.en_US hunspellDicts.en_US-large ])
    ];
  };

  programs = {
    mtr.enable = true;
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

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

/*
  sops = {
    defaultSopsFile = "${root}/secrets/secrets.yaml";
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

    secrets = {
      "${user.login}-pw".neededForUsers = true;
    };
  };
  */

  zramSwap.enable = true;
  system.stateVersion = lib.mkForce "23.05";
}
