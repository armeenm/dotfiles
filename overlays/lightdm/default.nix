{ pkgs, ... }:

pkgs.lightdm.overrideAttrs (old: {
  patches = old.patches ++ [ ./xsession.patch ./xauthority.patch ];
})
