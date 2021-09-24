{
  description = "Nix-based config";

  inputs = {
    stable.url = "github:nixos/nixpkgs/release-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus/1.3.0";
  };

  outputs =
    { self
    , stable
    , unstable
    , nur
    , home-manager
    , utils
    , ... } @ inputs:
    let
      ext = import ./ext { lib = unstable.lib; };
      root = ./.;
    in
      utils.lib.mkFlake {
        inherit self inputs;

        channelsConfig.allowUnfree = true;
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
          nur.overlay
        ];

        hosts = import ./hosts;
        hostDefaults = {
          extraArgs = { inherit ext root; };
          channelName = "unstable";

          modules = [
            ./modules
            home-manager.nixosModules.home-manager
          ];
        };

      } // utils.lib.eachDefaultSystem (system:
        let pkgs = unstable.legacyPackages."${system}";
        in { devShell = import ./shell.nix { inherit pkgs; }; }
      );
}
