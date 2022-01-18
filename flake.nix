{
  description = "Nix-based config";

  inputs = {
    stable.url = github:nixos/nixpkgs/release-21.11;
    unstable.url = github:nixos/nixpkgs/nixos-unstable;

    sops-nix.url = github:Mic92/sops-nix;
    sops-nix.inputs.nixpkgs.follows = "unstable";

    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "unstable";

    deploy-rs.url = github:serokell/deploy-rs;
    deploy-rs.inputs.nixpkgs.follows = "unstable";

    emacs-overlay.url = github:nix-community/emacs-overlay;
    utils.url = github:gytis-ivaskevicius/flake-utils-plus;
  };

  outputs =
    { self
    , stable
    , unstable
    , sops-nix
    , home-manager
    , deploy-rs
    , emacs-overlay
    , utils
    , ... } @ inputs:
    let
      root = ./.;
      ext = import ./ext { lib = unstable.lib; };

      domain = "armeen.org";
      user = {
        login = "armeen";
        name = "Armeen Mahdian";
        email = "mahdianarmeen@gmail.com";
      };

      deployNode = hostname: let
        config = self.nixosConfigurations."${hostname}";
        system = config.config.nixpkgs.system;
      in {
        hostname = "${hostname}.${domain}";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib."${system}".activate.nixos config;
        };
      };

    in utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig = {
        allowUnfree = true;
        contentAddressedByDefault = true;
      };

      channels = {
        unstable.overlaysBuilder = channels: [
          (_: _: { stable = channels.stable; })
          (_: _: { unstable = channels.unstable; })
        ];

        stable.overlaysBuilder = channels: [
          (_: _: { stable = channels.stable; })
          (_: _: { unstable = channels.unstable; })
        ];
      };

      overlay = import ./overlays;
      sharedOverlays = [
        self.overlay
        emacs-overlay.overlay
      ];

      img = {
        basic = unstable.lib.nixosSystem {
          system = "x86_64-linux";
          modules =  [
            "${unstable}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
            ./img/basic
          ];
        };
      };

      hosts = import ./hosts;
      hostDefaults = {
        channelName = "unstable";
        system = "x86_64-linux";
        extraArgs = { inherit ext root domain user; };

        modules = [
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
          ./modules
        ];
      };

      deploy.nodes = {
        cesium = deployNode "cesium";
        francium = deployNode "francium";
        hydrogen = deployNode "hydrogen";
      };

      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

    } // utils.lib.eachDefaultSystem (system:
      let pkgs = unstable.legacyPackages."${system}";
      in {
        devShell = pkgs.mkShell {
          packages = with pkgs; [
            deploy-rs.packages."${system}".deploy-rs
            git-crypt
            nixpkgs-fmt
            openssl
          ];
        };
      }
    );
}
