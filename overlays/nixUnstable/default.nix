{ pkgs, fetchFromGitHub, ... }:

pkgs.nixUnstable.overrideAttrs (_: {
  src = fetchFromGitHub {
    owner = "NixOS";
    repo = "nix";
    rev = "a2811f8499c8fa6e4da5b8b67940423fe269e974";
    hash = "sha256-kkLZ+8Nvm+hTxkN3Sq+FE8QcJpdvwHq9SydnY1rK3As=";
  };

  patches = [ ./pb.patch ];
})
