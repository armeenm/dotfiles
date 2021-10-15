{ config, pkgs, lib, inputs, ... }:

{
  imports = [ ./sys ./home ];
  
  system.stateVersion = lib.mkForce "21.05";

  nix = {
    trustedUsers = [ "root" "@wheel" ];
    trustedBinaryCaches = [ "https://hydra.iohk.io" ];
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    custom.flakes.enable = true;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "lm92" "nct6775" "tcp_bbr" ];

    kernelParams = [
      "slab_nomerge"
      "slub_debug=FZ"
      "init_on_alloc=1"
      "init_on_free=1"
      "page_alloc.shuffle=1"
      "pti=1"
      "vsyscall=none"
      "debugfs=off"
      #"oops=panic"
      #"module.sig_enforce=1"
      #"lockdown=confidentiality"
      "mce=0"
      "quiet loglevel=0"
      "spectre_v2=on"
      "spec_store_bypass_disable=on"
      "tsx=off"
      "tsx_async_abort=full,nosmt"
      "mds=full" # nosmt
      "l1tf=full,force"
      #"nosmt=force"
      "kvm.nx_huge_pages=force"
      "selinux=1"
      "security=selinux"
    ];

    extraModprobeConfig = ''
      options kvm_intel nested=1

      install dccp /bin/false
      install sctp /bin/false
      install rds /bin/false
      install tipc /bin/false
      install n-hdlc /bin/false
      install ax25 /bin/false
      install netrom /bin/false
      install x25 /bin/false
      install rose /bin/false
      install decnet /bin/false
      install econet /bin/false
      install af_802154 /bin/false
      install ipx /bin/false
      install appletalk /bin/false
      install psnap /bin/false
      install p8023 /bin/false
      install p8022 /bin/false
      install can /bin/false
      install atm /bin/false

      install cramfs /bin/false
      install freevxfs /bin/false
      install jffs2 /bin/false
      install hfs /bin/false
      install udf /bin/false

      install cifs /bin/true
      install nfs /bin/true
      install nfsv3 /bin/true
      install nfsv4 /bin/true
      install gfs2 /bin/true

      install vivid /bin/false
    '';

    kernel.sysctl = {
      "net.ipv4.conf.default.rp_filter" = 1;
      "net.ipv4.conf.all.rp_filter" = 1;
      "net.ipv4.icmp_echo_ignore_broadcasts" = 1;
      "net.ipv4.icmp_echo_ignore_all" = 1;
      "net.ipv4.icmp_ignore_bogus_error_responses" = 1;
      "net.ipv4.tcp_syncookies" = 1;
      "net.ipv4.tcp_synack_retries" = 5;
      "net.ipv4.conf.all.accept_redirects" = 0;
      "net.ipv4.conf.all.secure_redirects" = 0;
      "net.ipv4.conf.default.accept_redirects" = 0;
      "net.ipv4.conf.default.secure_redirects" = 0;
      "net.ipv4.conf.all.send_redirects" = 0;
      "net.ipv4.conf.all.accept_source_route" = 0;
      "net.ipv4.conf.default.accept_source_route" = 0;
      "net.ipv4.tcp_rfc1337" = 1;
      "net.ipv4.tcp_fastopen" = 3;
      "net.ipv4.tcp_congestion_control" = "bbr";
      "net.ipv4.ip_forward" = 0;
      "net.ipv4.tcp_sack" = 0;
      "net.ipv4.tcp_dsack" = 0;
      "net.ipv4.tcp_fack" = 0;
      "net.ipv4.conf.all.log_martians" = 1;

      "net.ipv6.conf.all.accept_redirects" = 0;
      "net.ipv6.conf.default.accept_redirects" = 0;
      "net.ipv6.conf.all.accept_source_route" = 0;
      "net.ipv6.conf.default.router_solicitations" = 0;
      "net.ipv6.conf.default.accept_ra" = 0;
      "net.ipv6.conf.default.accept_ra_rtr_pref" = 0;
      "net.ipv6.conf.default.accept_ra_pinfo" = 0;
      "net.ipv6.conf.default.aceept_ra_defrtr" = 0;
      "net.ipv6.conf.default.max_addresses" = 1;

      "net.core.default_qdisc" = "cake";
      "net.core.bpf_jit_harden" = 2;
      "net.core.rmem_max" = 8388608;
      "net.core.wmem_max" = 8388608;
      "net.core.netdev_max_backlog" = 5000;
      "net.ipv4.tcp_window_scaling" = 1;

      "kernel.core_uses_pid" = 1;
      "kernel.printk" = "3 3 3 3";
      "kernel.sysrq" = 4;
      "kernel.panic" = 10;
      "kernel.perf_event_paranoid" = 3;
      "kernel.kptr_restrict" = 2;
      "kernel.dmesg_restrict" = 1;
      "kernel.kexec_load_disabled" = 1;
      "kernel.yama.ptrace_scope" = 2;
      "kernel.unprivileged_bpf_disabled" = 1;
      "kernel.randomize_va_space" = 2;

      "vm.unprivileged_userfaultfd" = 0;
      # Appropriate for x86
      "vm.mmap_rnd_bits" = 32;
      "vm.mmap_rnd_compat_bits" = 16;

      "dev.tty.ldisc_autoload" = 0;

      #"user.max_user_namespaces" = 0;

      "fs.protected_hardlinks" = 1;
      "fs.protected_symlinks" = 1;
      "fs.protected_fifos" = 2;
      "fs.protected_regular" = 2;
    };

    tmpOnTmpfs = true;
    cleanTmpDir = true;
    
    custom.luks-yubikey = {
      enable = true;
      root = "/dev/disk/by-uuid/6d656974-8d5a-4820-a8c2-957f83ae5a2a";
      boot = config.fileSystems."/boot".device;
    };

    custom.efi.enable = true;
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

  security.rtkit.enable = true;

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

    tcsd.enable = false;

    journald = {
      extraConfig = ''
        ReadKMsg = no
      '';
    };

    openssh = {
      enable = true;
      forwardX11 = true;
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
        gutenprint gutenprintBin cnijfilter2
      ];
    };

    physlock = {
      enable = true;
      allowAnyUser = true;
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
    defaultPackages = lib.mkForce [];

    systemPackages = with pkgs; [
      rxvt_unicode.terminfo
      neovim
      lm_sensors
    ];

    variables.EDITOR = "nvim";
    
    pathsToLink = [ "/share/zsh" ];
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
    };
  };
}
