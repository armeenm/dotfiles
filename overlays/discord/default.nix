{ pkgs, ... }:

pkgs.discord.overrideAttrs (old: rec {
  version = "0.0.16";

  src = pkgs.fetchurl {
    url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
    sha256 = "UTVKjs/i7C/m8141bXBsakQRFd/c//EmqqhKhkr1OOk=";
  };
})
