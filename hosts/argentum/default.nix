{ lib, user, ... }:

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

  home-manager.users."${user.login}" = {
    services.wluma.enable = true;
  };

  networking.hostname = "argentum";

  nixpkgs.hostPlatform = "x86_64-linux";

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  programs.light.enable = true;

  services = {
    hardware.bolt.enable = true;
    xserver.videoDrivers = [ "intel" ];

    logind = {
      extraConfig = "HandlePowerKey=suspend";
      lidSwitch = "suspend";
    };
  };

  system.stateVersion = lib.mkForce "24.11";

  users.users."${user.login}".extraGroups = [
    "adbusers"
    "i2c"
    "lp" # Printing
    "plugdev"
    "scanner"
  ];

  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };
}
