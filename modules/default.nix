{ inputs, ... }:

let
  hmBase = [
    inputs.determinate.homeManagerModules.default
    inputs.direnv-instant.homeModules.direnv-instant
    inputs.nix-index-database.homeModules.nix-index
  ];

  hmLinuxBase = hmBase ++ [
    { nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ]; }
  ];

  nixosBase = [
    inputs.determinate.nixosModules.default
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
    { 
      nixpkgs.overlays = [ 
        inputs.darwin-emacs.overlays.emacs
        inputs.emacs-overlay.overlays.package
      ];
    }
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
  hmLinuxBase = { ... }: { imports = hmLinuxBase; };
  nixosBase = { ... }: { imports = nixosBase; };
  nixosInteractive = { ... }: { imports = nixosInteractive; };
  nixosUser = { ... }: { imports = [ ./nixos/user.nix ]; };

  hmDarwinBase = { ... }: { imports = hmDarwinBase; };
  darwinBase = { ... }: { imports = darwinBase; };
  darwinInteractive = { ... }: { imports = darwinInteractive; };
}
