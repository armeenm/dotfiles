{ lib
, inputs
, osConfig
, isHeadless
, isStandalone
, ...
}:

let
  inherit (osConfig.nixpkgs) hostPlatform;
in {
  imports = [
    ./home.nix
    ./programs.nix
    ./xdg.nix
  ] ++ lib.optionals (hostPlatform.isLinux) [
    ./services.nix
    ./systemd.nix
    ./wayland.nix
  ] ++ lib.optionals (!isHeadless && isStandalone) [
    ../shared/stylix.nix
  ];

  fonts.fontconfig.enable = !isHeadless;
  gtk.enable = !isHeadless;

  nix = lib.optionalAttrs isStandalone {
    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nixpkgs-stable.flake = inputs.nixpkgs-stable;
    };
  };
}
