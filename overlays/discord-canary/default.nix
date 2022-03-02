{ pkgs, ... }:

pkgs.discord-canary.overrideAttrs (old: rec {
  version = "0.0.134";

  src = pkgs.fetchurl {
    url = "https://dl-canary.discordapp.net/apps/linux/${version}/discord-canary-${version}.tar.gz";
    sha256 = "sha256-HyJa6lGcKMPKWffO/pnNcn8fDTJj6O4J8Y5RA23a1kM=";
  };
})
