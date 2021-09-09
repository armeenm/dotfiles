{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:nix-community/NUR/master";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
  };

  outputs =
    { self
    , nixpkgs
    , unstable
    , nur
    , utils
    , ... } @ inputs: utils.lib.systemFlake {
      inherit self inputs;

      channelsConfig.allowUnfree = true;
      channels.nixpkgs.overlaysBuilder = channels: [
        (final: prev: { unstable = channels.unstable; })
        (import ./overlays/mathematica.nix)
      ];

      hostDefaults.modules = [];

      hosts.lithium.modules = [
        ./hosts/lithium
      ];
    };
}
