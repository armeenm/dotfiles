{
  description = "Nix-based config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:nix-community/NUR/master";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , nur
    , ... } @ inputs: {
      nixosConfigurations = {
        lithium = import ./hosts/lithium inputs;
      };
    };
}
