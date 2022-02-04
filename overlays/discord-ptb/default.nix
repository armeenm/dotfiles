{ pkgs, ... }:

pkgs.discord-ptb.overrideAttrs (old: rec {
  version = "0.0.28";

  src = pkgs.fetchurl {
    url = "https://dl-ptb.discordapp.net/apps/linux/${version}/discord-ptb-${version}.tar.gz";
    sha256 = "sha256-+ButCM4l4WcKJBLZNQ10YiIt38NUCftN7dKrK9tig9I=";
  };
})
