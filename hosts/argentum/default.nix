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

  nix.buildMachines = [
    {
      hostName = "server0";
      protocol = "ssh-ng";
      sshUser = "it";
      sshKey = "/etc/nix/id_ed25519";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUdTQTJUdlI5amhNQXZ0czd6YzBDeHByVlhoeWYvcWU1Qkprb2I4Q1JyZ2Egcm9vdEBzZXJ2ZXIwCg==";
      maxJobs = 64;
      supportedFeatures = [
        "big-parallel"
        "kvm"
      ];
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];
    }
  ];

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
MIIDYzCCAuqgAwIBAgIUCSU6nXFXyXsstcjC8f2rxYLLGf4wCgYIKoZIzj0EAwIw
bTELMAkGA1UEBhMCSU4xFjAUBgNVBAgTDVV0dGFyIFByYWRlc2gxDjAMBgNVBAcT
BU5vaWRhMQ0wCwYDVQQKEwRFZFBpMQswCQYDVQQLEwJJVDEaMBgGA1UEAxMRRWRQ
aSBDb3Jwb3JhdGUgQ0EwHhcNMjYwMzA5MTE0ODA1WhcNMzUwOTA3MDQwNTU2WjAs
MSowKAYDVQQDEyFJbnRlcm1lZGlhdGUgQ0EgdjIgLSBXZWIgU2VydmljZXMwggEi
MA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC8qJreHJb2mYVriOSlKpt1Id7j
0YjGVVOx2JnAMA57D0rQUKMhL3BqF5awtwmiAliYRqHbSL0vDUEPlJuJaqJ/ogV/
zBqTleTkq7f1lpVIxu/68QK75ls6yp9t4ajNWKZA+E6RRPpAhM2kqOfbo0KoG6I+
su28TqO8tLTqNeEUjq1qNFKzCfVCJdzPgtF8HJd9oafVYlGSWNyl+4lPsCyaGOIH
k/BrCYjDETFw8q4u3/d3xd5MTRvcogsRpQKDGeTQGJGuD/YwMdYvT7yuTXRC3MUW
9GelEvUHwfm95cEQVO+CAai6t2HX/qSnw92tSvPGdpCHxHaiLoBUeRHEhhs7AgMB
AAGjgd0wgdowDgYDVR0PAQH/BAQDAgEGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0O
BBYEFB1Kd+/Y1es6PCdMeuxO2hctSZADMB8GA1UdIwQYMBaAFBiHCTtfN72hIw5S
Vst+cm8LbdEbMEAGCCsGAQUFBwEBBDQwMjAwBggrBgEFBQcwAoYkaHR0cHM6Ly92
YXVsdC5jb3JwLmVkcGkuYWkvdjEvcGtpL2NhMDUGA1UdHwQuMCwwKqAooCaGJGh0
dHA6Ly92YXVsdC5jb3JwLmVkcGkuYWkvdjEvcGtpL2NybDAKBggqhkjOPQQDAgNn
ADBkAjBxdVvAwnKu0H1Ut5aGc9faKAA4rp9I9IXiDTwFjvmPIK5DaNqhKMuxTQAx
Uq+9pXoCMFABEBJlZB7ywnsiEELZgWqkRomnB/dZygsEFn5YgFyMmHXYbUfo4RVK
EGqGIa9yWA==
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
