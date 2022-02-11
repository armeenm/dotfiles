{ pkgs, fetchFromGitHub, ... }:

pkgs.nixUnstable.overrideAttrs (_: {
  src = fetchFromGitHub {
    owner = "NixOS";
    repo = "nix";
    rev = "4d67ecbbb2a00b22b1b23073f5853bcb5b100b75";
    hash = "sha256-wce8M8AAFilvENjojAHZd62blCqvoGZcN5yUPB65g6A=";
  };

  patches = [ ./pb.patch ];
})
