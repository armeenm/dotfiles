{
  description = "Nix-based config";

  inputs = {
    stable.url = github:nixos/nixpkgs/nixos-22.05-small;
    unstable-small.url = github:nixos/nixpkgs/nixos-unstable-small;
    unstable.url = github:nixos/nixpkgs/nixos-unstable;
    master.url = github:nixos/nixpkgs;

    nur.url = github:nix-community/nur;

    sops-nix.url = github:Mic92/sops-nix;
    sops-nix.inputs.nixpkgs.follows = "unstable";

    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "unstable";

    deploy-rs.url = github:serokell/deploy-rs;
    deploy-rs.inputs.nixpkgs.follows = "unstable";

    emacs-overlay.url = github:nix-community/emacs-overlay;
    emacs-overlay.inputs.nixpkgs.follows = "unstable";

    nixpkgs-wayland.url = github:nix-community/nixpkgs-wayland;
    nixpkgs-wayland.inputs.nixpkgs.follows = "unstable";

    utils.url = github:gytis-ivaskevicius/flake-utils-plus;
  };

  outputs =
    { self
    , stable
    , unstable
    , utils
    , ...
    } @ inputs:
    let
      root = ./.;

      domain = "armeen.org";
      user = {
        login = "armeen";
        name = "Armeen Mahdian";
        email = "mahdianarmeen@gmail.com";
      };

      deployNode = hostname:
        let
          config = self.nixosConfigurations."${hostname}";
          system = config.config.nixpkgs.system;
        in
        {
          hostname = "${hostname}.${domain}";
          profiles.system = {
            user = "root";
            path = inputs.deploy-rs.lib."${system}".activate.nixos config;
          };
        };
    in
    utils.lib.mkFlake
      {
        inherit self inputs;

        channelsConfig = {
          allowUnfree = true;
          contentAddressedByDefault = false;
        };

        channels = {
          unstable.overlaysBuilder = channels: [
            (_: _: { stable = channels.stable; })
            (_: _: { unstable = channels.unstable; })
            (_: _: { master = channels.master; })
          ];

          unstable-small.overlaysBuilder = channels: [
            (_: _: { stable = channels.stable; })
            (_: _: { unstable = channels.unstable; })
            (_: _: { master = channels.master; })
          ];

          stable.overlaysBuilder = channels: [
            (_: _: { stable = channels.stable; })
            (_: _: { unstable = channels.unstable; })
            (_: _: { master = channels.master; })
          ];
        };

        overlay = import ./overlays;
        sharedOverlays = [
          self.overlay
          inputs.nur.overlay
          inputs.emacs-overlay.overlay
        ];

        img = {
          basic = stable.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              "${stable}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
              ./img/basic
            ];
          };

          gui = stable.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              "${stable}/nixos/modules/installer/cd-dvd/installation-cd-graphical-plasma5.nix"
            ];
          };

          gce = unstable.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              "${unstable}/nixos/modules/virtualisation/google-compute-image.nix"
            ];
          };
        };

        hostDefaults = {
          channelName = "unstable";
          system = "x86_64-linux";
          extraArgs = { inherit root domain user; };

          modules = [
            inputs.home-manager.nixosModules.home-manager
            inputs.sops-nix.nixosModules.sops
            inputs.nur.nixosModules.nur
            { nix.nixPath = [ "nixpkgs=${unstable}" ]; }
            ./modules
          ];
        };
        hosts = import ./hosts;

        # TODO: Fix `nix eval` + CA for these to work
        deploy.nodes = {
          cesium = deployNode "cesium";
          francium = deployNode "francium";
        };

        checks = builtins.mapAttrs
          (system: deployLib: deployLib.deployChecks self.deploy)
          inputs.deploy-rs.lib;

      } //
    utils.lib.eachDefaultSystem (system:
    let
      pkgs = {
        nix = unstable.legacyPackages.${system};
        deploy-rs = inputs.deploy-rs.packages.${system};
      };
    in {
      devShell = pkgs.nix.mkShell {
        packages = with pkgs.nix; [
          git-crypt
          gnupg
          google-cloud-sdk
          nixUnstable
          nixpkgs-fmt
          openssl
          pinentry-curses
        ] ++ (with pkgs.deploy-rs; [
          deploy-rs
        ]);

        shellHook = ''
          export PATH="$PWD/util:$PATH"
        '';
      };
    });
}
