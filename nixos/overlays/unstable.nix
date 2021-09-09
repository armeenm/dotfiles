{ nixpkgs-unstable, system }:

(final: prev: {
  unstable = nixpkgs-unstable.legacyPackages."${system}";
})
