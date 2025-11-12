{ inputs, ... }:

let
  hmBase = [
    inputs.direnv-instant.homeModules.direnv-instant
    inputs.nix-index-database.homeModules.nix-index
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

  hmDarwinBase = hmBase ++ [
    inputs.mac-app-util.homeManagerModules.default
  ];

  darwinBase = [
    inputs.home-manager.darwinModules.default
    inputs.ragenix.darwinModules.default
    { home-manager.sharedModules = hmDarwinBase; }
    ./darwin
  ];

  darwinInteractive = darwinBase ++ [
    inputs.stylix.darwinModules.stylix
    ./darwin/interactive.nix
  ];

in {
  hmBase = { ... }: { imports = hmBase; };
  nixosBase = { ... }: { imports = nixosBase; };
  nixosInteractive = { ... }: { imports = nixosInteractive; };
  nixosUser = { ... }: { imports = [ ./nixos/user.nix ]; };

  hmDarwinBase = { ... }: { imports = hmDarwinBase; };
  darwinBase = { ... }: { imports = darwinBase; };
  darwinInteractive = { ... }: { imports = darwinInteractive; };
}
