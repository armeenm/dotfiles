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
    , ... } @ inputs: utils.lib.mkFlake {
      inherit self inputs;

      overlay = import ./overlays;

      channelsConfig.allowUnfree = true;

      channels.nixpkgs.overlaysBuilder = channels: [
        (final: prev: { unstable = channels.unstable; })
      ];

      sharedOverlays = [
        self.overlay
        nur.overlay
      ];

      hostDefaults.modules = [
        ./modules
        home-manager.nixosModules.home-manager
      ];

      hosts = import ./hosts;
    };
}
