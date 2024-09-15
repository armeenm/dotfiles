{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11-small";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-misc = {
      url = "github:armeenm/nix-misc";
      inputs.nixpkgs.follows = "nixpkgs";
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
      inputs.home-manager.follows = "home-manager";
    };

    ragenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    brew-nix = {
      url = "github:BatteredBunny/brew-nix";
      inputs.brew-api.follows = "brew-api";
      inputs.nix-darwin.follows = "nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    brew-api = {
      url = "github:BatteredBunny/brew-api";
      flake = false;
    };

    mac-app-util = {
      url = "github:hraban/mac-app-util";
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }: let
    config = {
      allowUnfree = true;
      contentAddressedByDefault = false;
    };

    overlay = (import ./overlay { inherit inputs; });

    overlays = [
      overlay
      inputs.emacs-overlay.overlays.default
      inputs.brew-nix.overlays.default
    ];

    forAllSystems = f: nixpkgs.lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ] (system: f system (
      import nixpkgs { inherit system config overlays; }
    ));

    root = ./.;
    user = {
      login = "armeen";
      name = "Armeen Mahdian";
      email = "armeen@fulminous-hill.com";
    };

    baseModules = [
      { _module.args = { inherit inputs root user; }; }
    ];

    hmModules = baseModules;

    hmDarwinModules = hmModules ++ [
      inputs.mac-app-util.homeManagerModules.default
    ];

    sysModules = [
      { nixpkgs = { inherit config overlays; }; }
    ];

    droidModules = baseModules;

    darwinModules = baseModules ++ sysModules ++ [
      inputs.home-manager.darwinModules.default
      inputs.mac-app-util.darwinModules.default
      { home-manager.sharedModules = hmDarwinModules; }
      ./modules/darwin
    ];

    nixosModules = baseModules ++ sysModules ++ [
      inputs.home-manager.nixosModules.default
      inputs.lanzaboote.nixosModules.lanzaboote
      inputs.ragenix.nixosModules.default
      { home-manager.sharedModules = hmModules; }
      ./modules/nixos
    ];

  in {
    overlays.default = overlay;

    nixosConfigurations = {
      lithium = nixpkgs.lib.nixosSystem {
        modules = nixosModules ++ [
          ./hosts/lithium
        ];
      };

      /*
      argentum = nixpkgs.lib.nixosSystem {
        modules = modules ++ [
          ./hosts/argentum
          ./home
          inputs.nixos-hardware.nixosModules.framework-12th-gen-intel
        ];
      };
      */

      /*
      boron = nixpkgs.lib.nixosSystem {
        modules = modules ++ [
          ./hosts/boron
          ./home
        ];
      };
      */

      carbon = nixpkgs.lib.nixosSystem {
        modules = nixosModules ++ [
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

    darwinConfigurations = {
      rhenium = inputs.nix-darwin.lib.darwinSystem {
        modules = darwinModules ++ [
          ./hosts/rhenium
        ];
      };
    };

    homeConfigurations = forAllSystems (system: pkgs: with pkgs; {
      default = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = (if system == "aarch64-darwin" then hmDarwinModules else hmModules) ++ [
          { nixpkgs = { inherit config overlays; }; }
          ./home
        ];
      };
    });

    nixOnDroidConfigurations = {
      default = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import nixpkgs {
          inherit config;
          overlays = overlays ++ [ inputs.nix-on-droid.overlays.default ];
          system = "aarch64-linux";
        };
        modules = [ ./hosts/droid ];
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

        /*
        boron = {
          hostname = "boron";
          profiles.system = {
            user = "root";
            sudo = "doas -u";
            path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.boron;
          };
        };
        */
      };
    };

    devShells = forAllSystems (system: pkgs: with pkgs; {
      default = mkShell {
        packages = [
          inputs.deploy-rs.packages.${system}.default
          nix-output-monitor
          nvd
          openssl
        ];

        shellHook = ''
          export PATH=$PWD/util:$PATH
        '';
      };
    });

    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;
  };
}
