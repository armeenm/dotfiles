{ inputs, ... }:

final: prev: {
  stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};

  dufs = prev.dufs.overrideAttrs (_: {
    doCheck = false;
  });

  mozillavpn = prev.mozillavpn.overrideAttrs (_: {
    patches = [ ./mozillavpn.patch ];
  });

  swift = prev.swift.override { stdenv = final.clang19Stdenv; };
}
