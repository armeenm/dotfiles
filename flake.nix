{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.05-small";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    brew-api = {
      url = "github:BatteredBunny/brew-api";
      flake = false;
    };

    brew-nix = {
      url = "github:BatteredBunny/brew-nix";
      inputs.brew-api.follows = "brew-api";
      inputs.nix-darwin.follows = "nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      # Intentionally foregoing nixpkgs override.
    };

    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    hyprland-qtutils = {
      url = "github:hyprwm/hyprland-qtutils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprspace = {
      url = "github:KZDKM/Hyprspace";
      inputs.hyprland.follows = "hyprland";
    };

    hyprsplit = {
      url = "github:shezdy/hyprsplit";
      inputs.hyprland.follows = "hyprland";
    };

    hypr-darkwindow = {
      url = "github:micha4w/Hypr-DarkWindow";
      inputs.hyprland.follows = "hyprland";
    };

    hypr-dynamic-cursors = {
      url = "github:VirtCode/hypr-dynamic-cursors";
      inputs.hyprland.follows = "hyprland";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.4.2";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mac-app-util = {
      url = "github:hraban/mac-app-util";
    };

    nixgl = {
      url = "github:bb010g/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-misc = {
      url = "github:armeenm/nix-misc";
      inputs.nixpkgs.follows = "nixpkgs";
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

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }: let
    config = {
      allowUnfree = true;
      contentAddressedByDefault = false;
    };

    overlay = (import ./overlay { inherit inputs; });

    allOverlays = [
      inputs.brew-nix.overlays.default
      inputs.emacs-overlay.overlays.default
      inputs.nixgl.overlay
      overlay
    ];

    forAllSystems = f: nixpkgs.lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ] (system: f system (
      import nixpkgs {
        inherit system config;
        overlays = allOverlays;
      }
    ));

    user = {
      login = "armeen";
      name = "Armeen Mahdian";
      email = "armeen@fulminous-hill.com";
    };

    baseModules = [
      { _module.args = { inherit inputs user; }; }
      { nixpkgs = { inherit config; overlays = allOverlays; }; }
    ];

    /*
    darwinModules = baseModules ++ [
      inputs.home-manager.darwinModules.default
      inputs.mac-app-util.darwinModules.default
      inputs.stylix.darwinModules.stylix
      { home-manager.sharedModules = hmDarwinModules; }
      ./modules/darwin
    ];
*/

  in rec {
    nixosConfigurations = {
      lithium = nixpkgs.lib.nixosSystem {
        modules = baseModules ++ [
          nixosModules.nixosInteractive
          nixosModules.nixosUser
          inputs.lanzaboote.nixosModules.lanzaboote
          ./hosts/lithium
        ];
      };

      argentum = nixpkgs.lib.nixosSystem {
        modules = baseModules ++ [
          nixosModules.nixosInteractive
          nixosModules.nixosUser
          inputs.nixos-hardware.nixosModules.framework-12th-gen-intel
          ./hosts/argentum
        ];
      };

      carbon = nixpkgs.lib.nixosSystem {
        modules = baseModules ++ [
          nixosModules.nixosBase
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

    nixosModules = import ./modules { inherit inputs; };

    /*
    darwinConfigurations = {
      rhenium = inputs.nix-darwin.lib.darwinSystem {
        modules = darwinModules ++ [
          ./hosts/rhenium
        ];
      };
    };
*/

    homeConfigurations = forAllSystems (system: pkgs: {
      default = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = baseModules ++ [
          inputs.stylix.homeModules.stylix
          (if system == "aarch64-darwin" then nixosModules.hmDarwinBase else nixosModules.hmBase)
          ./home
        ];

        extraSpecialArgs = {
          isHeadless = false;
          isStandalone = true;
          isPortable = false;
          enableSocial = false;
          stateVersion = "24.11";
        };
      };
    });

    nixOnDroidConfigurations = {
      default = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import nixpkgs {
          inherit config;
          overlays = overlays ++ [ inputs.nix-on-droid.overlays.default ];
          system = "aarch64-linux";
        };
        modules = [
          inputs.stylix.nixOnDroidModules.default
          ./hosts/droid
        ];
      };
    };

    overlays.default = nixpkgs.lib.composeManyExtensions allOverlays;

    deploy = {
      nodes = {
        argentum = {
          hostname = "argentum";
          profiles.system = {
            user = "root";
            sudo = "doas -u";
            path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.argentum;
          };
        };

        carbon = {
          hostname = "carbon";
          profiles.system = {
            sshUser = "root";
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

    lib = {
      inherit forAllSystems;
    };
  };
}
