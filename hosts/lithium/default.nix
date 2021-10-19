{ config, pkgs, lib, inputs, root, ... }:

{
  imports = [
    ./home
    ./hw-generated.nix
  ];
  
  system.stateVersion = lib.mkForce "21.05";

  nix = {
    trustedUsers = [ "root" "@wheel" ];
    trustedBinaryCaches = [ "https://hydra.iohk.io" ];
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    custom.flakes.enable = true;
  };

  boot = {
    initrd = {
      availableKernelModules = [
        "uhci_hcd"
        "ehci_hcd"
        "ehci_pci"
        "ohci_hcd"
        "ohci_pci"
        "xhci_hcd"
        "xhci_pci"
        "usbhid"
        "hid_generic"
        "hid_lenovo"
        "hid_apple"
        "hid_roccat"
        "hid_logitech_hidpp"
        "hid_logitech_dj"
        "hid_microsoft"
      ];
      includeDefaultModules = false;
    };
    
    kernelPackages = pkgs.callPackage ./kernel.nix {};

    kernelModules = [
      "lm92"
      "nct6775"
      "tcp_bbr"
    ];

    kernelParams = [
      "slub_debug=FZ"
      "page_alloc.shuffle=1"
      "page_poison=1"
      "lockdown=confidentiality"
      "mce=0"
      "quiet loglevel=0"
      "spectre_v2=on"
      "spec_store_bypass_disable=on"
      "tsx=off"
      "tsx_async_abort=full,nosmt"
      "mds=full"
      "l1tf=full,force"
      "kvm.nx_huge_pages=force"
      "selinux=1"
      "security=selinux"
    ];

    blacklistedKernelModules = [
      "dccp"
      "sctp"
      "af_802154"
      "appletalk"
      "atm"
      "ax25"
      "can"
      "decnet"
      "econet"
      "ipx"
      "n-hdlc"
      "netrom"
      "p8022"
      "p8023"
      "psnap"
      "rds"
      "rose"
      "sctp"
      "sysv"
      "tipc"
      "x25"

      "adfs"
      "affs"
      "bfs"
      "befs"
      "cramfs"
      "efs"
      "erofs"
      "exofs"
      "freevxfs"
      "f2fs"
      "hfs"
      "hpfs"
      "jfs"
      "jffs2"
      "minix"
      "nilfs2"
      "omfs"
      "qnx4"
      "qnx6"
      "sysv"
      "udf"
      "ufs"

      "cifs"
      "gfs2"
      "nfs"
      "nfsv3"
      "nfsv4"
    ];

    extraModprobeConfig = ''
      options kvm_intel nested=1
    '';

    kernel.randstructSeed = "eee";

    kernel.sysctl = {
      "net.ipv4.conf.all.accept_redirects" = false;
      "net.ipv4.conf.all.accept_source_route" = false;
      "net.ipv4.conf.all.log_martians" = true;
      "net.ipv4.conf.all.rp_filter" = true;
      "net.ipv4.conf.all.secure_redirects" = false;
      "net.ipv4.conf.all.send_redirects" = false;
      "net.ipv4.conf.default.accept_redirects" = false;
      "net.ipv4.conf.default.accept_source_route" = false;
      "net.ipv4.conf.default.log_martians" = true;
      "net.ipv4.conf.default.rp_filter" = true;
      "net.ipv4.conf.default.secure_redirects" = false;
      "net.ipv4.icmp_echo_ignore_all" = true;
      "net.ipv4.icmp_echo_ignore_broadcasts" = true;
      "net.ipv4.icmp_ignore_bogus_error_responses" = true;
      "net.ipv4.ip_forward" = false;
      "net.ipv4.tcp_congestion_control" = "bbr";
      "net.ipv4.tcp_dsack" = false;
      "net.ipv4.tcp_fack" = false;
      "net.ipv4.tcp_fastopen" = 3;
      "net.ipv4.tcp_rfc1337" = true;
      "net.ipv4.tcp_sack" = false;
      "net.ipv4.tcp_synack_retries" = 5;
      "net.ipv4.tcp_timestamps" = false;
      "net.ipv4.tcp_window_scaling" = true;

      "net.ipv6.conf.all.accept_redirects" = false;
      "net.ipv6.conf.all.accept_source_route" = false;
      "net.ipv6.conf.all.secure_redirects" = false;
      "net.ipv6.conf.default.accept_ra" = false;
      "net.ipv6.conf.default.accept_ra_pinfo" = false;
      "net.ipv6.conf.default.accept_ra_rtr_pref" = false;
      "net.ipv6.conf.default.accept_redirects" = false;
      "net.ipv6.conf.default.aceept_ra_defrtr" = false;
      "net.ipv6.conf.default.max_addresses" = 1;
      "net.ipv6.conf.default.router_solicitations" = false;
      "net.ipv6.conf.default.secure_redirects" = false;

      "net.core.bpf_jit_harden" = 2;
      "net.core.default_qdisc" = "cake";
      "net.core.netdev_max_backlog" = 5000;
      "net.core.rmem_max" = 8388608;
      "net.core.wmem_max" = 8388608;

      "kernel.core_uses_pid" = true;
      "kernel.kptr_restrict" = 2;
      "kernel.panic_on_oops" = true;
      "kernel.printk" = "3 3 3 3";
      "kernel.randomize_va_space" = 2;
      "kernel.unprivileged_bpf_disabled" = true;
      "kernel.unprivileged_userns_clone" = true;
      "kernel.yama.ptrace_scope" = 2;

      # Appropriate for x86
      "vm.max_map_count" = 1048576;
      "vm.mmap_rnd_bits" = 32;
      "vm.mmap_rnd_compat_bits" = 16;

      "user.max_user_namespaces" = 10000;

      "fs.protected_fifos" = 2;
      "fs.protected_hardlinks" = true;
      "fs.protected_regular" = 2;
      "fs.protected_symlinks" = true;
    };

    tmpOnTmpfs = false;
    cleanTmpDir = true;

    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        editor = true;
      };
    };

    custom.luks-yubikey = {
      enable = true;
      root = "/dev/disk/by-uuid/6d656974-8d5a-4820-a8c2-957f83ae5a2a";
      boot = config.fileSystems."/boot".device;
    };
  };

  time.timeZone = "America/Chicago";

  networking = {
    hostName = "lithium";

    networkmanager.enable = true;
    wireguard.enable = true;

    firewall.checkReversePath = "loose";
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    keyMap = "us";
    font = "Tamsyn7x13r";
    packages = [ pkgs.tamsyn ];
    earlySetup = true;
  };

  security = {
    rtkit.enable = true;

    protectKernelImage = true;
    unprivilegedUsernsClone = false;
    forcePageTableIsolation = true;
    allowUserNamespaces = true;

    virtualisation = {
      flushL1DataCache = "cond";
    };

    tpm2 = {
      enable = true;
      abrmd.enable = true;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };

    doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
        keepEnv = true;
        noPass = true;
      }];
    };

    sudo = {
      enable = true;
      execWheelOnly = true;
      wheelNeedsPassword = false;
    };
  };

  fileSystems = {
    "/".options = [
      "noatime"
      "autodefrag"
    ];
  };

  zramSwap.enable = true;

  systemd = {
    tmpfiles.rules = [
      "d /tmp/nix 0755 root root"
    ];
  };


  services = {
    upower.enable = true;
    mullvad-vpn.enable = true;
    avahi.enable = true;
    blueman.enable = true;
    pcscd.enable = true;
    udisks2.enable = true;
    autorandr.enable = true;
    smartd.enable = true;
    nix-serve.enable = true;
    fstrim.enable = true;
    timesyncd.enable = false;
    tcsd.enable = false;
    physlock.enable = true;
    haveged.enable = true;

    chrony = {
      enable = true;
      enableNTS = true;
      servers = [
        "time.cloudflare.com"
      ];
    };

    journald = {
      extraConfig = ''
        ReadKMsg = no
      '';
    };

    openssh = {
      enable = true;
      forwardX11 = true;
    };

    usbguard = {
      enable = false;
      rules = builtins.readFile ./conf/usbguard/rules.conf;
    };
    
    ipfs = {
      enable = false;
      autoMount = true;
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint
        gutenprintBin
        cnijfilter2
      ];
    };

    udev = {
      packages = with pkgs; [
        ledger-udev-rules
        yubikey-personalization
      ];
    };

    tor = {
      enable = true;
      client.enable = true;
    };

    xserver = {
      enable = true;
      custom.noAccelInput.enable = true;
      custom.userXsession.enable = true;
      custom.autoLoginUser = "nixpower";

      layout = "us";
      xkbOptions = "caps:ctrl_modifier";

    };
  };

  hardware = {
    bluetooth.enable = true;
    custom.nvidia.enable = true;

    cpu = {
      amd.updateMicrocode = true;
      intel.updateMicrocode = true;
    };

    fancontrol = {
      enable = false;
      config = ''
      '';
    };
  };

  users = {
    #mutableUsers = false; TODO
    
    users.nixpower = {
      isNormalUser = true;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
        "networkmanager"
        "i2c"
        "adbusers"
        "libvirtd"
      ];
    };
  };

  environment = {
    # NOTE: Scudo may be preferable, but breaks Chromium audio
    memoryAllocator.provider = "graphene-hardened";

    pathsToLink = [ "/share/zsh" ];

    defaultPackages = lib.mkForce [];
    systemPackages = with pkgs; [
      rxvt_unicode.terminfo
    ];
  };

  programs = {
    adb.enable = true;

    custom.ddcutil = {
      enable = true;
      users = [ "nixpower" ];
    };

    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      defaultEditor = true;
    };
  };
}
