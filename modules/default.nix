{
  shared = import ./shared;
  darwin = import ./darwin;

  nixos = import ./nixos;
  nixosBase = import ./nixos/base.nix;
  nixosInteractive = import ./nixos/interactive.nix;
}
