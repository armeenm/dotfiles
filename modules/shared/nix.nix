{ config, lib, pkgs, inputs, ... }:

{
  nix = {
    distributedBuilds = true;
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nixpkgs-stable.flake = inputs.nixpkgs-stable;
    };

    settings = {
      auto-optimise-store = true;
      builders-use-substitutes = true;
      eval-cores = 0;
      sandbox = "relaxed";
      warn-dirty = false;
      # extra-builtins-file = ./extra-builtins.nix;

      # plugin-files = let
      #   inherit (pkgs.stdenv.hostPlatform) system;
      #   nix-plugins = pkgs.nix-plugins.override {
      #     nixComponents = inputs.nix.packages.${system};
      #   };
      # in [
      #   "${nix-plugins}/lib/nix/plugins"
      # ];

      experimental-features = [
        "auto-allocate-uids"
        "ca-derivations"
        "flakes"
        "impure-derivations"
        "nix-command"
        "recursive-nix"
      ];
    };
  };
}
