{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

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
    , home-manager
    , utils
    , ... } @ inputs: utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig.allowUnfree = true;
      channels.nixpkgs.overlaysBuilder = channels: [
        (final: prev: { unstable = channels.unstable; })
        (import ./overlays/mathematica.nix)
      ];

      hostDefaults.modules = [ ./modules ];

      hosts.lithium.modules = [ ./hosts/lithium ];

      homeConfigurations = {
        "nixpower" = home-manager.lib.homeManagerConfiguration {
          configuration = import ./home.nix;
          system = "x86_64-linux";
          homeDirectory = "/home/nixpower";
          username = "nixpower";
          stateVersion = "21.05";
        };
      };
    };
}
