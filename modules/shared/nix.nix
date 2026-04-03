{ config, lib, pkgs, inputs, ... }:

{
  nix = {
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

      experimental-features = [
        "auto-allocate-uids"
        #"ca-derivations"
        "flakes"
        "impure-derivations"
        "nix-command"
        "recursive-nix"
      ];
    };
  };
}
