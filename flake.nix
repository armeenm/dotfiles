{
  description = "Nix-based config";

  inputs = {
    stable.url = github:nixos/nixpkgs/release-21.05;
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
      user = {
        login = "armeen";
        name = "Armeen Mahdian";
        email = "mahdianarmeen@gmail.com";
      };
    in
      utils.lib.mkFlake {
        inherit self inputs;

        channelsConfig = {
          allowUnfree = true;
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
        sharedOverlays = [
          self.overlay
        ];

        hosts = import ./hosts;
        hostDefaults = {
          extraArgs = { inherit ext root user; };
          channelName = "unstable";

          modules = [
            ./modules
            agenix.nixosModules.age
            home-manager.nixosModules.home-manager
          ];
        };

        img = {
          basic = unstable.lib.nixosSystem {
            system = "x86_64-linux";
            modules =  [
              "${unstable}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
              ./img/basic
            ];
          };
        };


      } // utils.lib.eachDefaultSystem (system:
        let pkgs = unstable.legacyPackages."${system}";
        in {
          devShell = pkgs.mkShell {
            packages = with pkgs; [
              nixpkgs-fmt
            ];
          };
        }
      );
}
