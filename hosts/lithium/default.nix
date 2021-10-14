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
    kernelModules = [ "lm92" "nct6775" ];
    extraModprobeConfig = "options kvm_intel nested=1";

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

    fancontrol = {
      enable = false;
      config = ''
      '';
    };
  };

  users.users.nixpower = {
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
