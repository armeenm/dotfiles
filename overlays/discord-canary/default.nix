{ pkgs, ... }:

pkgs.discord-canary.overrideAttrs (old: rec {
  version = "0.0.133";

  src = pkgs.fetchurl {
    url = "https://dl-canary.discordapp.net/apps/linux/${version}/discord-canary-${version}.tar.gz";
    sha256 = "sha256-CqwWLedPh4KCXEQAGM0lFlAyULRtKLVIAonvP9/kqHM=";
  };
})
