{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-26.05-small";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    nixpkgs-patcher.url = "github:gepbird/nixpkgs-patcher";

    brew-api = {
      url = "github:armeenm/brew-api";
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

    direnv-instant = {
      url = "github:Mic92/direnv-instant";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix = {
      url = "https://flakehub.com/f/DeterminateSystems/nix-src/3.21.8";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    determinate = {
      url = "github:DeterminateSystems/determinate";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix.follows = "nix";
    };

    darwin-emacs = {
      url = "github:nix-giant/nix-darwin-emacs";
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

    hyprland-qtutils = {
      url = "github:hyprwm/hyprland-qtutils";
      inputs.nixpkgs.follows = "nixpkgs";
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
      url = "github:nix-community/lanzaboote/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mac-app-util = {
      url = "github:hraban/mac-app-util";
      # Use our (unstable) nixpkgs so mac-app-util is built with a fixed SBCL:
      # upstream's nixos-26.05 pin still ships SBCL 2.6.4, which fails to
      # allocate its heap on macOS 27 ("failed to allocate ... at 0x300100000").
      inputs.nixpkgs.follows = "nixpkgs";
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

    rose-pine-hyprcursor = {
      url = "github:armeenm/rose-pine-hyprcursor";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    strace-macos = {
      url = "github:Mic92/strace-macos";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-patcher, ... }: let
    inherit (nixpkgs) lib;

    config = {
      allowUnfree = true;
      contentAddressedByDefault = false;
    };

    overlay = (import ./overlay { inherit inputs lib; });

    allOverlays = [
      inputs.brew-nix.overlays.default
      inputs.nixgl.overlay
      overlay
    ];

    # Overlays for the bare nixpkgs instances built here (devShells,
    # homeConfigurations, nixOnDroidConfigurations). The NixOS and nix-darwin
    # modules add the emacs overlays themselves, but these standalone outputs
    # don't go through those modules (and so don't inherit `nixpkgs.overlays`),
    # while `home/programs.nix` needs `emacsWithPackagesFromUsePackage`.
    standaloneOverlays = system:
      allOverlays
      ++ [ inputs.emacs-overlay.overlays.default ]
      ++ lib.optionals (system == "aarch64-darwin") [
        # Must come last, same as modules/darwin/base.nix: this is the
        # `emacsWithPackagesFromUsePackage` that understands
        # `configAsDefaultInitFile`.
        inputs.darwin-emacs.overlays.emacs
      ];

    forAllSystems = f: lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ] (system: f system (
      import nixpkgs {
        inherit system config;
        overlays = standaloneOverlays system;
      }
    ));

    user = {
      login = "armeen";
      name = "Armeen Mahdian";
      email = "armeen@fulminous-hill.com";
    };

    commonModules = [
      { _module.args = { inherit inputs user; }; }
    ];

    baseModules = commonModules ++ [
      { nixpkgs = { inherit config; overlays = allOverlays; }; }
    ];

  in rec {
    nixosModules = import ./modules { inherit inputs; };

    nixosConfigurations = {
      lithium = nixpkgs-patcher.lib.nixosSystem {
        modules = baseModules ++ [
          nixosModules.nixosInteractive
          nixosModules.nixosUser
          inputs.lanzaboote.nixosModules.lanzaboote
          ./hosts/lithium
        ];

        specialArgs = inputs;
      };

      argentum = nixpkgs-patcher.lib.nixosSystem {
        modules = baseModules ++ [
          nixosModules.nixosInteractive
          nixosModules.nixosUser
          inputs.nixos-hardware.nixosModules.framework-12th-gen-intel
          inputs.lanzaboote.nixosModules.lanzaboote
          ./hosts/argentum
        ];

        specialArgs = inputs;
      };

      carbon = nixpkgs-patcher.lib.nixosSystem {
        modules = baseModules ++ [
          nixosModules.nixosBase
          nixosModules.nixosUser
          ./hosts/carbon
        ];

        specialArgs = inputs;
      };

      hydrogen = nixpkgs-patcher.lib.nixosSystem {
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
          ./hosts/hydrogen
        ];

        specialArgs = inputs;
      };
    };

    darwinConfigurations = rec {
      itmaclap = cadmium;

      cadmium = inputs.nix-darwin.lib.darwinSystem {
        specialArgs = inputs;

        modules = baseModules ++ [
          nixosModules.darwinInteractive
          ./hosts/cadmium
        ];
      };

      rhenium = inputs.nix-darwin.lib.darwinSystem {
        specialArgs = inputs;

        modules = baseModules ++ [
          nixosModules.darwinInteractive
          ./hosts/rhenium
        ];
      };
    };

    configurations = nixosConfigurations // darwinConfigurations;

    homeConfigurations = forAllSystems (system: pkgs:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # `homeManagerConfiguration` already takes overlays from `pkgs`
        # (`pkgs.overlays`, which come from `standaloneOverlays`). Adding
        # `baseModules` would apply the local overlay a second time, and
        # overlays like nix-output-monitor's append a patch each time.
        modules = commonModules ++ [
          inputs.stylix.homeModules.stylix
          (if system == "aarch64-darwin"
           then nixosModules.hmDarwinBase
           else nixosModules.hmBase)
          ./home
        ];

        extraSpecialArgs = {
          isHeadless = false;
          isStandalone = true;
          isPortable = false;
          enableSocial = false;
          stateVersion = "24.11";
          cursorColor = null;
          cursorSize = null;
        };
      }
    );

    nixOnDroidConfigurations = {
      default = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import nixpkgs {
          inherit config;
          overlays = standaloneOverlays "aarch64-linux" ++ [ inputs.nix-on-droid.overlays.default ];
          system = "aarch64-linux";
        };
        modules = [
          inputs.stylix.nixOnDroidModules.default
          ./hosts/droid
          # We hand nix-on-droid an already-overlaid `pkgs` below, so stylix
          # must not also set `nixpkgs.overlays` (which its flake mode forbids).
          { stylix.overlays.enable = false; }
        ];
      };
    };

    overlays.default = lib.composeManyExtensions allOverlays;

    deploy = {
      nodes = with self.nixosConfigurations; let
        inherit (inputs.deploy-rs) lib;
      in {
        argentum = {
          hostname = "argentum";
          profiles.system = {
            sshUser = "root";
            path = lib.x86_64-linux.activate.nixos argentum;
          };
        };

        carbon = {
          hostname = "carbon";
          profiles.system = {
            sshUser = user.login;
            user = "root";
            interactiveSudo = true;
            path = lib.x86_64-linux.activate.nixos carbon;
          };
        };

        lithium = {
          hostname = "lithium";
          profiles.system = {
            sshUser = "root";
            path = lib.x86_64-linux.activate.nixos lithium;
          };
        };

      };
    };

    devShells = forAllSystems (system: pkgs: with pkgs; {
      default = (mkShell.override { stdenv = pkgs.stdenvNoCC; }) {
        packages = [
          (inputs.deploy-rs.packages.${system}.default.overrideAttrs (_: {
            # deploy-rs' canary/file-watching tests are timing dependent and
            # fail in the Darwin build sandbox, so skip them there.
            doCheck = system != "aarch64-darwin";
          }))
          nix-output-monitor
          nvd
          openssl
        ];

        shellHook = ''
          export PATH=$PWD/util:$PATH
          unset CONFIG_SHELL \
            HOST_PATH \
            MACOSX_DEPLOYMENT_TARGET \
            NIX_BUILD_CORES \
            NIX_BUILD_TOP \
            NIX_CFLAGS_COMPILE \
            NIX_DONT_SET_RPATH \
            NIX_DONT_SET_RPATH_FOR_BUILD \
            NIX_ENFORCE_NO_NATIVE \
            NIX_IGNORE_LD_THROUGH_GCC \
            NIX_NO_SELF_RPATH \
            NIX_STORE \
            PATH_LOCALE \
            SOURCE_DATE_EPOCH \
            TEMP \
            TEMPDIR \
            TMP \
            __darwinAllowLocalNetworking \
            __impureHostDeps \
            __propagatedImpureHostDeps \
            __propagatedSandboxProfile \
            __sandboxProfile \
            __structuredAttrs \
            buildInputs \
            buildPhase \
            builder \
            cmakeFlags \
            configureFlags \
            depsBuildBuild \
            depsBuildBuildPropagated \
            depsBuildTarget \
            depsBuildTargetPropagated \
            depsHostHost \
            depsHostHostPropagated \
            depsTargetTarget \
            depsTargetTargetPropagated \
            doCheck \
            doInstallCheck \
            dontAddDisableDepTrack \
            mesonFlags \
            name \
            nativeBuildInputs \
            out \
            outputs \
            p \
            patches \
            phases \
            preferLocalBuild \
            propagatedBuildInputs \
            propagatedNativeBuildInputs \
            shell \
            shellHook \
            stdenv \
            strictDeps \
            system
        '';
      };
    });

    # deploy-rs' `lib` covers every system it knows about, including ones
    # nixpkgs no longer supports (x86_64-darwin in 26.11), so only produce
    # checks for the systems this flake targets.
    checks = lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ] (system: inputs.deploy-rs.lib.${system}.deployChecks self.deploy);

    lib = nixpkgs.lib // {
      inherit forAllSystems;
    };
  };
}
