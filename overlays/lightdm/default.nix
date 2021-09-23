{ pkgs, ... }:

pkgs.lightdm.overrideAttrs (old: {
  patches = old.patches ++ [ ./xsession.diff ];
})
