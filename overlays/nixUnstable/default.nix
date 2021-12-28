{ pkgs, fetchFromGitHub, ... }:

pkgs.nixUnstable.override {
  src = fetchFromGitHub {
    owner = "NixOS";
    repo = "nix";
    rev = "0e90b13ab1df925e549b5d55853b65911b4b40d3";
    hash = "sha256-YlfeZ+dV73B7kQo7NIjhQNHgUHFBG80zOJLxD161yxU=";
  };
}
