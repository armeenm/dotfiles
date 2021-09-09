inputs @ { nixpkgs, nixpkgs-unstable, ... }:

let
  system = "x86_64-linux";

  #pkgs = import nixpkgs {
  #  inherit system;
  #  config.allowUnfree = true;
  #  overlays = [];
  #};

  #pkgs' = import nixpkgs-unstable {
  #  inherit system;
  #  config.allowUnfree = true;
  #  overlays = [];
  #};

  overlay-unstable = final: prev: {
    unstable = nixpkgs-unstable.legacyPackages.x86_64-linux;
  };

  overlays = 
in
  nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [
      #{ nixpkgs.overlays = [ overlay-unstable (import ../../overlays/mathematica.nix) ]; }
      ./configuration.nix
    ];
  }
