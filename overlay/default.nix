{ inputs, ... }:

final: prev: {
  stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};

  direnv = prev.direnv.overrideAttrs (old: {
    doCheck = false;
  });

  mozillavpn = prev.mozillavpn.overrideAttrs (old: {
    patches = [ ./foo.patch ];
  });
}
