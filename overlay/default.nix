{ inputs, lib, ... }:

final: prev: {
  stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};

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

  fwupd = prev.fwupd.overrideAttrs (old: {
    mesonFlags = old.mesonFlags ++ [
      (lib.mesonOption "efi_app_location" "/run/fwupd-efi")
    ];
  });

  openchamber-desktop = final.callPackage ./openchamber.nix { };
}
