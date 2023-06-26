{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable-small;
    nixpkgs-stable.url = github:nixos/nixpkgs/nixos-23.05-small;

    deploy-rs.url = github:serokell/deploy-rs;
    home-manager.url = github:nix-community/home-manager;
    nixos-hardware.url = github:nixos/nixos-hardware;
    nur.url = github:nix-community/nur;
    sops-nix.url = github:Mic92/sops-nix;
    nix-misc.url = github:armeenm/nix-misc;

    emacs-overlay.url = github:nix-community/emacs-overlay;
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    hyprland.url = github:hyprwm/Hyprland;
    hyprland.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, ... }: let
    config = {
      allowUnfree = true;
      contentAddressedByDefault = false;
    };

    overlays = [
      (import ./overlay)
      inputs.emacs-overlay.overlays.default
    ];

    forAllSystems = f: nixpkgs.lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
    ] (system: f system (
      import nixpkgs { inherit system config overlays; }
    ));

    root = ./.;
    user = rec {
      login = "armeen";
      name = "Armeen Mahdian";
      email = "armeen@fulminous-hill.com";
    };

    modules = [
      inputs.home-manager.nixosModules.home-manager
      inputs.hyprland.nixosModules.default
      inputs.nur.nixosModules.nur
      inputs.sops-nix.nixosModules.sops
      { nixpkgs = { inherit config overlays; }; }
      { _module.args = { inherit inputs root user; }; }
      ./modules
    ];

  in {
    nixosConfigurations = {
      lithium = nixpkgs.lib.nixosSystem {
        modules = modules ++ [
          ./hosts/lithium
          ./homes/full
        ];
      };

      argentum = nixpkgs.lib.nixosSystem {
        modules = modules ++ [
          ./hosts/argentum
          ./homes/full
          inputs.nixos-hardware.nixosModules.framework-12th-gen-intel
        ];
      };

      carbon = nixpkgs.lib.nixosSystem {
        modules = modules ++ [
          ./hosts/carbon
        ];
      };

      basic-img = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
          ./img/basic
        ];
      };
    };

    deploy = {
      nodes = {
        carbon = {
          hostname = "carbon";
          profiles.system = {
            user = "root";
            sudo = "doas -u";
            path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.carbon;
          };
        };

        lithium = {
          hostname = "lithium";
          profiles.system = {
            user = "root";
            sudo = "doas -u";
            path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.lithium;
          };
        };
      };
    };

    devShells = forAllSystems (system: pkgs: with pkgs; {
      default = mkShell {
        packages = [
          inputs.deploy-rs.packages.${system}.default
          nixUnstable
          openssl
          sops
        ];

        shellHook = ''
          export PATH=$PWD/util:$PATH
        '';
      };
    });

    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;
  };
}
