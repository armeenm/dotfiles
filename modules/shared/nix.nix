{ config, lib, pkgs, inputs, ... }:

{
  nix = {
    package = lib.mkForce (
      let
        overrideComponents = _final: prev: {
          # Meson resolves a source path beginning with the build directory's
          # name as build-dir-relative (generated). `src/libstore` has a real
          # `build/` source directory that collides with meson's default
          # `build` build directory, so rename the build directory for just
          # this component. (Other components assume the default name.)
          nix-store = prev.nix-store.overrideAttrs (_: {
            mesonBuildDir = "_build";
          });
        };
      in
      if pkgs.stdenv.hostPlatform.isDarwin then
        # Build the components from our own nixpkgs, so the overlay's libgit2
        # (tests disabled in the macOS sandbox) is used.
        ((inputs.nix.lib.makeComponents { inherit pkgs; }).overrideScope overrideComponents)
        .nix-everything.overrideAttrs (_: {
          doCheck = false;
        })
      else
        (inputs.nix.packages."${pkgs.stdenv.system}".default.overrideScope overrideComponents)
        .overrideAttrs (_: {
          doCheck = false;
        })
    );

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
