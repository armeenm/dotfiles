{ config, pkgs, lib, modulesPath, inputs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  system.stateVersion = "21.05"; # Don't change
  
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs.config = {
    allowUnfree = true;

    mullvad-vpn = pkgs.mullvad-vpn.overrideAttrs (old: rec {
      version = "2020.7";
      src = pkgs.fetchurl {
        url = "https://www.mullvad.net/media/app/MullvadVPN-${version}_amd64.deb";
        sha256 = "07vryz1nq8r4m5y9ry0d0v62ykz1cnnsv628x34yvwiyazbav4ri";
      };
    });

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
      extraRules = builtins.readFile "${pkgs.ddcutil}/share/ddcutil/data/45-ddcutil-i2c.rules";
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
      inputs.nixpkgs-unstable.pkgs.torbrowser

      (mathematica.overrideAttrs (old: rec {
        version = "12.3.1";
        lang = "en";

        name = "mathematica-${version}" + lib.optionalString (lang != "en") "-${lang}";
        src = requireFile rec {
          name = "Mathematica_${version}" + lib.optionalString (lang != "en") "_${language}" + "_LINUX.sh";
          message = ''
            This nix expression requires that ${name} is
            already part of the store. Find the file on your Mathematica CD
            and add it to the nix store with nix-store --add-fixed sha256 <FILE>.
          '';
          sha256 = "51b9cab12fd91b009ea7ad4968a2c8a59e94dc55d2e6cc1d712acd5ba2c4d509";
        };
      }))
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
