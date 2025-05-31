{ inputs, ... }:

let
  hmBase = [
    inputs.nix-index-database.hmModules.nix-index
  ];

  hmDarwinBase = hmBase ++ [
    inputs.mac-app-util.homeManagerModules.default
  ];

  nixosBase = [
    inputs.home-manager.nixosModules.default
    inputs.ragenix.nixosModules.default
    { home-manager.sharedModules = hmBase; }
    ./nixos
  ];

  nixosInteractive = nixosBase ++ [
    inputs.stylix.nixosModules.stylix
    ./nixos/interactive.nix
  ];

in {
  hmBase = { ... }: { imports = hmBase; };
  hmDarwinBase = { ... }: { imports = hmDarwinBase; };
  nixosBase = { ... }: { imports = nixosBase; };
  nixosInteractive = { ... }: { imports = nixosInteractive; };
}
