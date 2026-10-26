{ config, inputs, lib, nixosModules, pkgs, user, ... }:

{
  nix.extraOptions = ''
    experimental-features = nix-command flakes ca-derivations
  '';

  environment.packages = with pkgs; [ neovim ];

  system.stateVersion = "24.05";

  home-manager = {
    backupFileExtension = "hm-bak";
    useGlobalPkgs = true;

    extraSpecialArgs = {
      inherit inputs user;
      stateVersion = config.system.stateVersion;
      isHeadless = true;
      isStandalone = false;
      isPortable = false;
      enableSocial = false;
      cursorColor = null;
      cursorSize = null;
    };

    config.imports = [
      nixosModules.hmBase
      ../../home
      { systemd.user.enable = false; }
    ];
  };

  # We hand nix-on-droid an already-overlaid `pkgs`, so stylix must not also
  # set `nixpkgs.overlays` (which its flake mode forbids).
  stylix.overlays.enable = false;
}
