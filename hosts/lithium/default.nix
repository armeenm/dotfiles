{ config, pkgs, lib, inputs, ... }:

{
  imports = [ ./hardware-configuration.nix ./home.nix ];
  
  system.stateVersion = lib.mkForce "21.05";

  nix.custom.flakes.enable = true;

  boot = {
    custom.luks-yubikey = {
      enable = true;
      boot = "/dev/nvme0n1p1";
      root = "/dev/nvme0n1p2";
    };

    custom.efi.enable = true;
  };

  time.timeZone = "America/Chicago";

  networking = {
    hostName = "lithium";

    networkmanager.enable = true;
    iproute2.enable = false;
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

  services = {
    upower.enable = true;
    mullvad-vpn.enable = true;
    avahi.enable = true;
    blueman.enable = true;
    pcscd.enable = true;
    udisks2.enable = true;
    #tcsd.enable = true;

    openssh = {
      enable = true;
      forwardX11 = true;
    };

    ipfs = {
      enable = true;
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

      layout = "us";
      xkbOptions = "caps:ctrl_modifier";

      custom.noAccelInput.enable = true;
      custom.userXsession.enable = true;
      custom.autoLoginUser = "nixpower";
    };
  };

  hardware = {
    bluetooth.enable = true;
    custom.nvidia.enable = true;
  };

  users.users.nixpower = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "i2c" "adbusers" ];
  };

  environment = {
    defaultPackages = lib.mkForce [];

    systemPackages = with pkgs; [
      neovim
      unstable.mathematica
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
