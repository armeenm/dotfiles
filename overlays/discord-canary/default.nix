{ pkgs, ... }:

pkgs.discord-canary.overrideAttrs (old: rec {
  version = "0.0.135";

  src = pkgs.fetchurl {
    url = "https://dl-canary.discordapp.net/apps/linux/${version}/discord-canary-${version}.tar.gz";
    hash = "sha256-dmG+3BWS1BMHHQAv4fsXuObVeAJBeD+TqnyQz69AMac=";
  };
})
