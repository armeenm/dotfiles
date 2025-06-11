{ lib, user, pkgs, ... }:

{
  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    kernelModules = [ "kvm-intel" ];
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/81BA-80F5";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

    "/" = {
      device = "/dev/disk/by-uuid/98affdf5-1020-4752-994e-343092303139";
      fsType = "ext4";
    };
  };

  hardware = {
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
    sensor.iio.enable = true;
  };

  home-manager = {
    extraSpecialArgs.isPortable = true;

    users."${user.login}" = {
      systemd.user.services = {
        mpvpaper = lib.mkForce {};
        rclone-cobalt = lib.mkForce {};
        rclone-oxygen = lib.mkForce {};
      };

      services = {
        wluma.enable = true;
        safeeyes.enable = lib.mkForce false;
      };

      wayland.windowManager.hyprland.settings.monitor = lib.mkForce [ ",preferred,auto,1.25" ];
    };
  };

  networking = {
    hostName = "argentum";
    networkmanager.wifi.powersave = true;
  };

  nixpkgs.hostPlatform = "x86_64-linux";

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  programs.fw-fanctrl.enable = true;

  services = {
    hardware.bolt.enable = true;
    xserver.videoDrivers = [ "intel" "displaylink" "modesetting" ];

    logind = {
      powerKey = "suspend";
      lidSwitch = "suspend";
    };
  };

  system.stateVersion = lib.mkForce "24.11";

  users.users."${user.login}".extraGroups = [
    "adbusers"
    "i2c"
    "input"
    "lp" # Printing
    "networkmanager"
    "plugdev"
    "scanner"
  ];

  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };

  environment.systemPackages = with pkgs; [
    displaylink
  ];

  systemd.services.dlm.wantedBy = [ "multi-user.target" ];
}
