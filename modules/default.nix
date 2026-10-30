{ inputs, ... }:

let
  hmBase = [
    inputs.direnv-instant.homeModules.direnv-instant
    inputs.nix-index-database.homeModules.nix-index
    inputs.ragenix.homeManagerModules.default
  ];

  # base is the common subset; full adds home-manager and the desktop stack,
  # while minimal is the headless appliance profile.
  nixosBase = [ ./nixos/base.nix ];

  nixosFull = [
    inputs.determinate.nixosModules.default
    inputs.home-manager.nixosModules.default
    inputs.niks3.nixosModules.default
    inputs.ragenix.nixosModules.default
    { home-manager.sharedModules = hmBase; }
    ./shared
    ./nixos/full.nix
  ];

  nixosInteractive = nixosFull ++ [
    inputs.stylix.nixosModules.stylix
    ./nixos/interactive.nix
  ];

  nixosMinimal = [ ./nixos/minimal.nix ];

  nixosRpi = [ ./nixos/rpi.nix ];

  hmDarwinBase = hmBase ++ [
    inputs.mac-app-util.homeManagerModules.default
  ];

  darwinBase = [
    inputs.determinate.darwinModules.default
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
  nixosFull = { ... }: { imports = nixosFull; };
  nixosInteractive = { ... }: { imports = nixosInteractive; };
  nixosMinimal = { ... }: { imports = nixosMinimal; };
  nixosRpi = { ... }: { imports = nixosRpi; };
  nixosUser = { ... }: { imports = [ ./nixos/user.nix ]; };

  hmDarwinBase = { ... }: { imports = hmDarwinBase; };
  darwinBase = { ... }: { imports = darwinBase; };
  darwinInteractive = { ... }: { imports = darwinInteractive; };
}
