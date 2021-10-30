{ config, pkgs, lib, inputs, root, ... }:

{
  imports = [
    ./home
    ./hw-generated.nix
  ];
  
  system.stateVersion = lib.mkForce "21.05";

  nix = {
    trustedUsers = [ "root" "@wheel" ];
    custom.flakes.enable = true;
  };

  boot = {
    initrd = {
      includeDefaultModules = false;
    };
    
    kernelPackages = pkgs.callPackage ./kernel.nix {};

    kernelParams = [
      "quiet"
      "loglevel=0"
      "page_alloc.shuffle=1"
      "lockdown=confidentiality"
      "kvm.nx_huge_pages=force"
      #"slub_debug=FZ"
    ];

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
      "kernel.panic_on_oops" = false;
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
    sudo.enable = false;

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
  };

  fileSystems = {
    "/".options = [
      "noatime"
      "ssd"
    ];
  };

  zramSwap.enable = true;

  systemd = {
    tmpfiles.rules = [
      "d /tmp/nix 0755 root root"
    ];
  };

  #location = {
  #  provider = "manual";
  #  latitude = 
  #  longitude =
  #};

  services = {
    autorandr.enable = true;
    blueman.enable = true;
    fstrim.enable = true;
    haveged.enable = true;
    mullvad-vpn.enable = true;
    nix-serve.enable = true;
    pcscd.enable = true;
    physlock.enable = true;
    smartd.enable = true;
    tcsd.enable = false;
    timesyncd.enable = false;
    udisks2.enable = true;
    redshift.enable = true;

    chrony = {
      enable = true;
      enableNTS = true;
      servers = [
        "time.cloudflare.com"
      ];
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
    cpu.amd.updateMicrocode = true;
    custom.nvidia.enable = true;
  };

  users = {
    mutableUsers = false;

    users = {
      root.hashedPassword = "*";

      nixpower = {
        isNormalUser = true;
        hashedPassword =
          "$6$D9bjWz87ZjX4AY3Q$vFQLpKIVHktTAdco3FW35ki5dKWtkiMH2h3uSgOUV5nYS2KVPVYBHtP2vkvbiJDbMReWF8jfHfWyw74Q/jBhs1";
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
  };

  environment = {
    # NOTE: Scudo may be preferable, but breaks Chromium audio
    #memoryAllocator.provider = "graphene-hardened";

    pathsToLink = [ "/share/zsh" ];

    defaultPackages = lib.mkForce [];
    systemPackages = with pkgs; [
      git
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
