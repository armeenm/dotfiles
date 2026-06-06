{ inputs, ... }:

final: prev: {
  stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};

  dufs = prev.dufs.overrideAttrs (old: {
    doCheck = false;
  });

  mozillavpn = prev.mozillavpn.overrideAttrs (old: {
    patches = [ ./foo.patch ];
  });
}
