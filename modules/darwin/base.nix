{ config, inputs, lib, pkgs, user, ... }:

let
  tailscaleVersion = "1.104.4";

  # The official macOS app, pinned so a rebuild is reproducible. The app
  # self-updates via Sparkle after installation; bump this when we want a
  # newer initial install.
  tailscalePkg = pkgs.fetchurl {
    url = "https://pkgs.tailscale.com/stable/Tailscale-${tailscaleVersion}-macos.pkg";
    hash = "sha256-7ogA9ll6zQQKtF8/7OIcuJ+2kZaKai4qMNHy8nB7QdI=";
  };
in
{
  # No more support for x86 macOS.
  nixpkgs = {
    hostPlatform = "aarch64-darwin";
    overlays = [
      # Provides `emacs-unstable` and refreshed ELPA/MELPA package sets.
      inputs.emacs-overlay.overlays.default
      # Must come last: this `emacsWithPackagesFromUsePackage` is the one that
      # understands `configAsDefaultInitFile`, which the shared home config
      # passes on Darwin.
      inputs.darwin-emacs.overlays.emacs
    ];
  };

  # Assume Determinate Nix on macOS.
  determinateNix = {
    enable = true;
    distributedBuilds = true;
    registry = config.nix.registry;

    determinateNixd = {
      builder.state = "enabled";
    };

    customSettings = config.nix.settings // {
      # Nix defaults sandboxing off on Darwin; keep builds sandboxed.
      sandbox = true;
      allowed-users = lib.mkForce [ "@staff" ];
      trusted-users = lib.mkForce [ "@admin" ];
    };
  };

  power.restartAfterFreeze = true;

  services = {
    # The official macOS app below replaces the open-source tailscaled on
    # Darwin. Keep services.tailscale disabled: running both conflicts, and
    # tailscaled-on-macOS is what has the exit-node/default-route bug.
    emacs = {
      enable = true;
      package = config.home-manager.users.${user.login}.programs.emacs.package;
    };
  };

  users.users.${user.login} = {
    home = "/Users/${user.login}";
  };

  environment.systemPackages = [
    # The app's CLI is inside the bundle and launches the GUI unless this is
    # set. Keep `tailscale` on PATH with the same name as before.
    (pkgs.writeShellScriptBin "tailscale" ''
      export TAILSCALE_BE_CLI=1
      exec /Applications/Tailscale.app/Contents/MacOS/Tailscale "$@"
    '')
  ];

  system.activationScripts.postActivation.text = lib.mkAfter ''
    # Install the official Tailscale macOS app (standalone/System Extension
    # variant) if it is not present. The pkg is pinned above; the app
    # self-updates after that.
    if [ ! -d /Applications/Tailscale.app ]; then
      # Make sure the open-source daemon is gone before the app installs; the
      # two variants must not run at the same time.
      launchctl bootout system/com.tailscale.tailscaled 2>/dev/null || true
      rm -f /etc/resolver/ts.net

      echo "installing Tailscale ${tailscaleVersion}..." >&2
      /usr/sbin/installer -pkg ${tailscalePkg} -target /
    fi
  '';
}
