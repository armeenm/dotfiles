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
    extraSpecialArgs = {
      isPortable = true;
      enableSocial = true;
      cursorColor = "#b89b51";
    };

    users."${user.login}" = {
      wayland.windowManager.hyprland.settings.monitor = lib.mkForce [ ",preferred,auto,1.25" ];

      systemd.user.services = {
        mpvpaper = lib.mkForce {};
        rclone-cobalt = lib.mkForce {};
        rclone-oxygen = lib.mkForce {};
      };

      services = {
        #wluma.enable = true;
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
    firewall.enable = false;
  };

  nixpkgs.hostPlatform = "x86_64-linux";

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  #programs.fw-fanctrl.enable = true;

  services = {
    cloudflare-warp.enable = true;
    dbus.packages = [ pkgs.miraclecast ];
    hardware.bolt.enable = true;
    xserver.videoDrivers = [ "intel" "modesetting" ];
    flatpak.enable = true;

    logind.settings.Login = {
      HandlePowerKey = "suspend";
      HandleLidSwitch = "suspend";
    };

    kmonad = {
      enable = true;
      keyboards.bulitin = {
        device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
        config = builtins.readFile ./builtin.kbd;
        defcfg = {
          enable = true;
          fallthrough = true;
        };
      };
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
    "podman"
    "scanner"
    "wireshark"
  ];

  environment.systemPackages = with pkgs; [
    #displaylink
  ];

  # Start DisplayLink Manager on boot.
  #systemd.services.dlm.wantedBy = [ "multi-user.target" ];

  security.pki.certificateFiles = [
    (pkgs.writeText "foo-web-int-ca" ''
-----BEGIN CERTIFICATE-----
MIIDYTCCAuegAwIBAgIUW/hLLLUarUzuRyLFhDykhz98iVMwCgYIKoZIzj0EAwIw
bTELMAkGA1UEBhMCSU4xFjAUBgNVBAgTDVV0dGFyIFByYWRlc2gxDjAMBgNVBAcT
BU5vaWRhMQ0wCwYDVQQKEwRFZFBpMQswCQYDVQQLEwJJVDEaMBgGA1UEAxMRRWRQ
aSBDb3Jwb3JhdGUgQ0EwHhcNMjUwOTA5MDQzNDQ1WhcNMjYwMzA3MDgzNTE1WjAp
MScwJQYDVQQDEx5JbnRlcm1lZGlhdGUgQ0EgLSBXZWIgU2VydmljZXMwggEiMA0G
CSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC6Fh+3Y32SBZuLHnsKTi8ld1JesRIU
33tx8qOm3ApZlM89KI1wlMFHw9V69610cjIWcoLOd8kqEwbhr4NvJSOMm0IKTVdz
c7Ou03+ZonDeA5OJLVRVBRijGVb8m6ei11w63JoYWsaQ9i1p2ypUlEFCuCpmv6g4
XX8i5/q4RA61zqmNcqGQq+2YAMuAaNYMdFRPdpinPhQ3sFF0SVehPFLzSkthpthf
f1b2Vys58jDPCZAO2O7xo3ME4DYqYo8fJ7uz79GbNcrGLzR0nwKKMX09u6PolSdo
3Htr6XEDtgdfqmyLpj2JJCsC9Cc0PoAXpBjCJcGgJfRWPWtWn0qdHrSRAgMBAAGj
gd0wgdowDgYDVR0PAQH/BAQDAgEGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYE
FKijDr8auTERKaufUGhx0J/Rt7cGMB8GA1UdIwQYMBaAFBiHCTtfN72hIw5SVst+
cm8LbdEbMEAGCCsGAQUFBwEBBDQwMjAwBggrBgEFBQcwAoYkaHR0cHM6Ly92YXVs
dC5jb3JwLmVkcGkuYWkvdjEvcGtpL2NhMDUGA1UdHwQuMCwwKqAooCaGJGh0dHA6
Ly92YXVsdC5jb3JwLmVkcGkuYWkvdjEvcGtpL2NybDAKBggqhkjOPQQDAgNoADBl
AjB15tfOyu0xSJMdDUY4iXZERGlPkZLO6Ug6ar8Alr9bnZ+mAlm6H5TcqVk0h5UN
OXUCMQDhxJUZm1QpRyzr1s7L9pZWx8JewbFABEGLq1UAggSbGm29DC2E2/+gRwzC
Hocfvms=
-----END CERTIFICATE-----
-----BEGIN CERTIFICATE-----
MIIDEDCCApagAwIBAgIUBqi0LKpIgwKkpnPXAFHw7Uq4F0QwCgYIKoZIzj0EAwMw
bTELMAkGA1UEBhMCSU4xFjAUBgNVBAgTDVV0dGFyIFByYWRlc2gxDjAMBgNVBAcT
BU5vaWRhMQ0wCwYDVQQKEwRFZFBpMQswCQYDVQQLEwJJVDEaMBgGA1UEAxMRRWRQ
aSBDb3Jwb3JhdGUgQ0EwHhcNMjUwOTA5MDQwNTI2WhcNMzUwOTA3MDQwNTU2WjBt
MQswCQYDVQQGEwJJTjEWMBQGA1UECBMNVXR0YXIgUHJhZGVzaDEOMAwGA1UEBxMF
Tm9pZGExDTALBgNVBAoTBEVkUGkxCzAJBgNVBAsTAklUMRowGAYDVQQDExFFZFBp
IENvcnBvcmF0ZSBDQTB2MBAGByqGSM49AgEGBSuBBAAiA2IABNTwARJ7hkeJG/lU
ZOuq5qKzoNYdQkJbQ+MrMsc4BgwnB6ftAS1hYFBV+7VydNLBeegt2wiQSeBEYg/Z
t7QBKiiFKFR9F3RZmwZTgNoPopl+hmJre1DR1fPUSmB8BS9NoqOB9jCB8zAOBgNV
HQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUGIcJO183vaEj
DlJWy35ybwtt0RswHwYDVR0jBBgwFoAUGIcJO183vaEjDlJWy35ybwtt0RswQAYI
KwYBBQUHAQEENDAyMDAGCCsGAQUFBzAChiRodHRwczovL3ZhdWx0LmNvcnAuZWRw
aS5haS92MS9wa2kvY2EwFwYDVR0RBBAwDoYMY29ycC5lZHBpLmFpMDUGA1UdHwQu
MCwwKqAooCaGJGh0dHA6Ly92YXVsdC5jb3JwLmVkcGkuYWkvdjEvcGtpL2NybDAK
BggqhkjOPQQDAwNoADBlAjA92edh9BXo1FBKzEK+C+jvBuivVRpVpR7cq9nu2GmS
LpLvpYbBQxHV9LaTnCDueIECMQDTxSOWEKIHVCymLIfCOYAzVstXzBH3MhMILnKo
SD+S6lUahwO5k1Tr7zsYxf4H/7o=
-----END CERTIFICATE-----
    '')
  ];
}
