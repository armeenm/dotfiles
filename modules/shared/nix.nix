{ config, lib, pkgs, inputs, ... }:

{
  nix = {
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nixpkgs-stable.flake = inputs.nixpkgs-stable;
    };

    settings = {
      allowed-users = lib.mkForce [ "@users" "@wheel" ];
      trusted-users = lib.mkForce [ "@wheel" ];
      eval-cores = 0;

      experimental-features = [
        "auto-allocate-uids"
        "ca-derivations"
        "flakes"
        "impure-derivations"
        "nix-command"
        "recursive-nix"
      ];

      warn-dirty = false;
    };
  };
}
