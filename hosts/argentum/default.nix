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
      wayland.windowManager.hyprland.settings.monitor = lib.mkForce [ ",preferred,auto,1.25" ];

      systemd.user.services = {
        mpvpaper = lib.mkForce {};
        rclone-cobalt = lib.mkForce {};
        rclone-oxygen = lib.mkForce {};
      };

      services = {
        wluma.enable = true;
        safeeyes.enable = lib.mkForce false;

        shikane = {
          enable = true;
          settings = {
            profile = [
              {
                name = "default";
                output = [
                  {
                    match = "eDP-1";
                    enable = true;
                    scale = 1.25;
                  }
                ];
              }
              {
                name = "docked";
                output = [
                  {
                    match = "eDP-1";
                    enable = true;
                    scale = 1.25;
                    position = "0,1273";
                  }
                  {
                    search = [ "m=MSI MD271UL" "s=PB8H065500413" "v=Microstep" ];
                    enable = true;
                    scale = 1.0;
                    position = "2304,0"; # 2880 (xres) / 1.25 (scale)
                  }
                ];
              }
            ];
          };
        };
      };
    };
  };

  networking = {
    hostName = "argentum";
    networkmanager.wifi.powersave = true;
  };

  nixpkgs.hostPlatform = "x86_64-linux";

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  #programs.fw-fanctrl.enable = true;

  services = {
    cloudflare-warp.enable = true;
    hardware.bolt.enable = true;
    xserver.videoDrivers = [ "intel" "displaylink" "modesetting" ];

    logind.settings.Login = {
      HandlePowerKey = "suspend";
      HandleLidSwitch = "suspend";
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
    "wireshark"
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
