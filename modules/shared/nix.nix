{ config, lib, pkgs, inputs, ... }:

{
  nix = {
    package = pkgs.nixVersions.latest;
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs = {
        flake = inputs.nixpkgs;
        to = inputs.nixpkgs;
      };
      nixpkgs-stable = {
        flake = inputs.nixpkgs-stable;
        to = inputs.nixpkgs-stable;
      };
    };

    settings = {
      allowed-users = lib.mkForce [ "@users" "@wheel" ];
      trusted-users = lib.mkForce [ "@wheel" ];

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
