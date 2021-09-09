{ config, pkgs, lib, inputs, ... }:

{
  system.stateVersion = "21.05"; # Don't change
  
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
  };

  boot = {
    initrd = {
      kernelModules = [
        "vfat"
        "nls_cp437"
        "nls_iso8859-1"
        "usbhid"
        "nvidia"
        "nvidia_modeset"
        "nvidia_uvm"
        "nvidia_drm"
      ];

      luks = {
        yubikeySupport = true;
        devices = {
          "nixos-enc" = {
            device = "/dev/nvme0n1p2";
            preLVM = true;
            yubikey = {
              slot = 2;
              twoFactor = true;
              storage = {
                device = "/dev/nvme0n1p1";
              };
            };
          };
        };
      };
    };

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelModules = [ "i2c-dev" "i2c-piix4" ];
  };

  time.timeZone = "America/Chicago";

  networking = {
    hostName = "lithium";

    networkmanager.enable = true;
    iproute2.enable = true;
    wireguard.enable = false; # TODO
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
      extraRules = builtins.readFile
        "${pkgs.ddcutil}/share/ddcutil/data/45-ddcutil-i2c.rules";

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
      videoDrivers = [ "nvidia" ];

      displayManager = {
        lightdm.greeter.enable = false;

        autoLogin = {
          enable = true;
          user = "nixpower";
        };
      };

      desktopManager.session = [
        {
          name = "user-xsession";
          start = ''
            ${pkgs.runtimeShell} $HOME/.xsession &
            waitPID=$!
          '';
        }
      ];

      xkbOptions = "caps:ctrl_modifier";

      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
        mouse.accelSpeed = "0";
        touchpad.accelProfile = "flat";
        touchpad.accelSpeed = "0";
      };
    };
  };

  hardware = {
    bluetooth.enable = true;
    nvidia.modesetting.enable = true;
  };

  users.users.nixpower = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "i2c" "adbusers" ];
  };

  environment = {
    systemPackages = with pkgs; [
      neovim
      rxvt-unicode
      mathematica
      unstable.torbrowser
    ];

    defaultPackages = lib.mkForce [];
    variables.EDITOR = "nvim";
    pathsToLink = [ "/share/zsh" ];
  };

  programs = {
    adb.enable = true;
    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
    };
  };
}
