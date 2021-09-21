{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:nix-community/NUR";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    utils.url = "github:gytis-ivaskevicius/flake-utils-plus/1.3.0";
  };

  outputs =
    { self
    , nixpkgs
    , unstable
    , nur
    , home-manager
    , utils
    , ... } @ inputs:
    let
      ext = import ./ext { lib = nixpkgs.lib; };
    in
      utils.lib.mkFlake {
        inherit self inputs;

        channelsConfig.allowUnfree = true;

        channels.nixpkgs.overlaysBuilder = channels: [
          (_: _: { unstable = channels.unstable; })
        ];

        overlay = import ./overlays;

        sharedOverlays = [
          self.overlay
          nur.overlay
        ];

        hosts = import ./hosts;

        hostDefaults.modules = [
          ./modules
          home-manager.nixosModules.home-manager
        ];

        hostDefaults.extraArgs = { inherit ext; };
      };
}
