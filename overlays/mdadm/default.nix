{ pkgs, stdenv, lib, ... }:

pkgs.mdadm.overrideAttrs (old: {
  makeFlags = old.makeFlags ++ [
    "RUN_DIR=/run/mdadm"
    "CHECK_RUN_DIR=0"
  ];
})
