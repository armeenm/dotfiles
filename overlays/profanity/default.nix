{ pkgs, fetchFromGitHub, libstrophe, ... }:

let
  rev = "7a8f0e487370d952560585e414e83441de602bd6";
in
pkgs.profanity.overrideAttrs (old: {
  version = "0.12.0dev.master.${builtins.substring 0 8 rev}";
  
  src = fetchFromGitHub {
    inherit rev;
    owner = "profanity-im";
    repo = old.pname;
    hash = "sha256-dyR+uUEe6pjZH0+7XCse/JC8/69vTXkYqrPBKBSXTzc=";
  };

  patches = [ ./packagestatus.patch ];
  buildInputs = old.buildInputs ++ [ libstrophe ];
})
