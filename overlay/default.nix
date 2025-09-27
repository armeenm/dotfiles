{ inputs, ... }:

final: prev: {
  stable = inputs.nixpkgs-stable.legacyPackages.${final.system};

  miraclecast = (prev.miraclecast.overrideAttrs (_: {
    version = "1.0-unstable-2025-09-21";
    src = prev.fetchFromGitHub {
      owner = "albfan";
      repo = "miraclecast";
      rev = "993b6d637d9c16b7a0115d6d1ef45b9d996aadd7";
      hash = "sha256-RjibKQbPrhCwl0r+/yJg5geRIlkdw64MqUWRuc//5jI=";
    };
  })).override {
    readline = prev.readline.overrideAttrs (old: {
      patches = old.patches ++ [
        (final.fetchpatch {
          name = "display-null-prompt.patch";
          url = "https://gitlab.archlinux.org/archlinux/packaging/packages/readline/-/raw/8.3.001-1/8.3.0-display-null-prompt.patch";
          stripLen = 2;
          hash = "sha256-QSS1GUJ2i/bF2ksvUtw27oqFHuTHALi+7QwxMFt9ZaM=";
        })
      ];
    });
  };
}
