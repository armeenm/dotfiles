{ inputs, ... }:

final: prev: {
  stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};

  mozillavpn = prev.mozillavpn.overrideAttrs (old: {
    patches = [ ./foo.patch ];
  });
}
