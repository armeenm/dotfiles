{ pkgs, ... }:

pkgs.whatsapp-for-linux.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [ pkgs.glib-networking ];
})
