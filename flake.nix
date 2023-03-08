{
  description = "Nix-based config";

  inputs = {
    stable.url = github:nixos/nixpkgs/nixos-22.11-small;
    unstable.url = github:nixos/nixpkgs/nixos-unstable-small;
    master.url = github:nixos/nixpkgs;

    nur.url = github:nix-community/nur;
    sops-nix.url = github:Mic92/sops-nix;
    home-manager.url = github:nix-community/home-manager;

    emacs-overlay.url = github:nix-community/emacs-overlay;
    emacs-overlay.inputs.nixpkgs.follows = "unstable";

    hyprland.url = github:hyprwm/Hyprland;
    hyprland.inputs.nixpkgs.follows = "unstable";

    nixos-hardware.url = github:nixos/nixos-hardware;

    utils.url = github:numtide/flake-utils;
  };

  outputs = inputs@{ self, ... }:
    inputs.utils.lib.eachDefaultSystem (system:
      let
        defaultFlake = inputs.unstable;
        config = {
          allowUnfree = true;
          contentAddressedByDefault = false;
        };

        overlays = [
          (import ./overlay)
          inputs.emacs-overlay.overlays.default
        ];

        pkgs = rec {
          default = import defaultFlake {
            inherit config system overlays;
          };

          stable = import inputs.stable {
            inherit config system overlays;
          };

          unstable = import inputs.unstable {
            inherit config system overlays;
          };

          master = import inputs.master {
            inherit config system overlays;
          };

          stdenv = default.stdenvNoCC;
          mkShell = default.mkShell.override { inherit stdenv; };
        };
        lib = defaultFlake.lib;

        root = ./.;
        domain = "armeen.org";
        user = rec {
          login = "armeen";
          name = "Armeen Mahdian";
          email = "${login}@${domain}";
        };

        modules = [
          inputs.home-manager.nixosModules.home-manager
          inputs.hyprland.nixosModules.default
          inputs.nur.nixosModules.nur
          inputs.sops-nix.nixosModules.sops
          { nixpkgs = { inherit config overlays; }; }
          { _module.args = { inherit inputs root domain user; }; }
          ./modules
        ];

    in {
      packages = {
        inherit pkgs;

        nixosConfigurations = {
          lithium = inputs.unstable.lib.nixosSystem {
            modules = modules ++ [
              ./hosts/lithium
              ./homes/full
            ];
          };

          argentum = inputs.unstable.lib.nixosSystem {
            modules = modules ++ [
              ./hosts/argentum
              ./homes/full
	            inputs.nixos-hardware.nixosModules.framework-12th-gen-intel
            ];
          };

          basic-img = inputs.unstable.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              "${inputs.unstable}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
              ./img/basic
            ];
          };
        };
      };

      devShells.default = pkgs.mkShell {
        packages = with pkgs.default; [
          nixUnstable
          openssl
          sops
        ];

        shellHook = ''
          export PATH=$PWD/util:$PATH
        '';
      };
    });
}
