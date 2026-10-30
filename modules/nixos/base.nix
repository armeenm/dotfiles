{ lib, user, ... }:

# Settings shared by the full interactive systems and the minimal appliances.
# Only put things here that both profiles need.
{
  environment.defaultPackages = lib.mkForce [ ];

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    keyMap = "us";
    earlySetup = false;
  };

  nix = {
    # Keep the store bounded. `--delete-older-than` implies `-d`, so this drops
    # every generation older than two weeks and leaves the rest alone.
    gc = {
      automatic = true;
      dates = "weekly";
      randomizedDelaySec = "30min";
      options = "--delete-older-than 14d";
    };

    settings = {
      # Wheel users may push locally built (and therefore unsigned) closures
      # over SSH; signature checking still applies to substituters.
      trusted-users = lib.mkForce [ "@wheel" ];
    };
  };

  # The firewall is enabled per host, but tailnet traffic is encrypted and
  # authenticated, so let it through wherever the NixOS firewall is in charge.
  networking.firewall.trustedInterfaces = [ "tailscale0" ];

  security = {
    protectKernelImage = true;

    sudo-rs.enable = true;
  };

  services = {
    fstrim.enable = true;

    openssh = {
      enable = true;

      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "prohibit-password";
      };
    };

    # Keep the clock close to real time; TLS and Tailscale both care. chrony
    # is a real NTP client rather than systemd's SNTP implementation.
    chrony.enable = true;
  };

  users = {
    mutableUsers = false;

    users = {
      root.hashedPassword = null;

      "${user.login}" = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
      };
    };
  };
}
