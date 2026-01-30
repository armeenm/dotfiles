{ config, lib, pkgs, inputs, ... }:

{
  nix = {
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nixpkgs-stable.flake = inputs.nixpkgs-stable;
    };

    settings = {
      eval-cores = 0;
      warn-dirty = false;
      sandbox = true;

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
