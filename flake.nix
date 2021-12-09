{
  description = "Nix-based config";

  inputs = {
    stable.url = github:nixos/nixpkgs/release-21.11;
    unstable.url = github:nixos/nixpkgs/nixos-unstable;

    agenix.url = github:ryantm/agenix;
    agenix.inputs.nixpkgs.follows = "unstable";

    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "unstable";

    deploy-rs.url = github:serokell/deploy-rs;
    emacs-overlay.url = github:nix-community/emacs-overlay;
    utils.url = github:gytis-ivaskevicius/flake-utils-plus/1.3.0;
  };

  outputs =
    { self
    , stable
    , unstable
    , agenix
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

    in utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig = {
        allowUnfree = true;
        allowBroken = true;
        contentAddressedByDefault = false;
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
      sharedOverlays = [ self.overlay ];

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
        extraArgs = { inherit ext root domain user; };

        modules = [
          home-manager.nixosModules.home-manager
          agenix.nixosModules.age
          ./modules
        ];
      };

      deploy.nodes = {
        francium = let
          config = self.nixosConfigurations.francium;
          system = config.config.nixpkgs.system;
        in {
          hostname = "francium.${domain}";
            
            profiles.system = {
              user = "root";
              path = deploy-rs.lib."${system}".activate.nixos config;
            };
        };
      };

      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

    } // utils.lib.eachDefaultSystem (system:
      let pkgs = unstable.legacyPackages."${system}";
      in {
        devShell = pkgs.mkShell {
          packages = with pkgs; [
            deploy-rs.packages."${system}".deploy-rs
            nixpkgs-fmt
          ];
        };
      }
    );
}
