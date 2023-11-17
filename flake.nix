{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.05-small";

    deploy-rs.url = "github:serokell/deploy-rs";
    home-manager.url = "github:nix-community/home-manager";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    nix-misc = {
      url = "github:armeenm/nix-misc";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mpd-mpris = {
      url = "github:natsukagami/mpd-mpris";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    update-systemd-resolved = {
      url = "github:jonathanio/update-systemd-resolved";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }: let
    config = {
      allowUnfree = true;
      contentAddressedByDefault = false;
    };

    overlays = [
      (import ./overlay { inherit inputs; })
      inputs.emacs-overlay.overlays.default
    ];

    forAllSystems = f: nixpkgs.lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
    ] (system: f system (
      import nixpkgs { inherit system config overlays; }
    ));

    root = ./.;
    user = {
      login = "armeen";
      name = "Armeen Mahdian";
      email = "armeen@fulminous-hill.com";
    };

    hmModules = [
      inputs.mpd-mpris.homeManagerModules.default
      { _module.args = { inherit inputs root user; }; }
    ];

    modules = hmModules ++ [
      inputs.home-manager.nixosModules.home-manager
      inputs.sops-nix.nixosModules.sops
      inputs.lanzaboote.nixosModules.lanzaboote
      inputs.update-systemd-resolved.nixosModules.update-systemd-resolved
      { nixpkgs = { inherit config overlays; }; }
      ./modules
    ];

  in {
    nixosConfigurations = {
      lithium = nixpkgs.lib.nixosSystem {
        modules = modules ++ [
          ./hosts/lithium
          ./home
        ];
      };

      argentum = nixpkgs.lib.nixosSystem {
        modules = modules ++ [
          ./hosts/argentum
          ./home
          inputs.nixos-hardware.nixosModules.framework-12th-gen-intel
        ];
      };

      boron = nixpkgs.lib.nixosSystem {
        modules = modules ++ [
          ./hosts/boron
          ./home
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

    homeConfigurations = forAllSystems (system: pkgs: with pkgs; {
      foo = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = hmModules ++ [ ./home ];
      };
    });

    nixOnDroidConfigurations = {
      default = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        modules = modules ++ [
          ./hosts/droid
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

        boron = {
          hostname = "boron";
          profiles.system = {
            user = "root";
            sudo = "doas -u";
            path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.boron;
          };
        };
      };
    };

    devShells = forAllSystems (system: pkgs: with pkgs; {
      default = mkShell {
        packages = [
          inputs.deploy-rs.packages.${system}.default
          nil
          nixUnstable
          nvd
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
