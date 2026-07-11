{ inputs, ... }:

final: prev: {
  stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};

  # TODO: Pinned until https://github.com/NixOS/nixpkgs/issues/540320 is fixed.
  age-plugin-se = final.stable.age-plugin-se;

  dufs = prev.dufs.overrideAttrs (_: {
    doCheck = false;
  });

  # TODO: Remove when https://github.com/shezdy/hyprsplit/issues/87 is fixed upstream.
  hyprsplit = inputs.hyprsplit.packages.${final.stdenv.hostPlatform.system}.hyprsplit.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./hyprsplit.patch ];
  });

  mozillavpn = prev.mozillavpn.overrideAttrs (_: {
    patches = [ ./mozillavpn.patch ];
  });
}
