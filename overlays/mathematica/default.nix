{ pkgs, lib, stdenv, ... }:

with lib;

pkgs.mathematica.overrideAttrs (old: rec {
  version = "13.0.0";
  lang = "en";
  language = "English";
  name = "mathematica-${version}" + optionalString (lang != "en") "-${lang}";

  buildInputs = [];
  nativeBuildInputs = with pkgs; old.buildInputs ++ [
  ];

  ldpath = makeLibraryPath nativeBuildInputs
           + optionalString (stdenv.hostPlatform.system == "x86_64-linux")
             (":" + makeSearchPathOutput "lib" "lib64" nativeBuildInputs);

  src = pkgs.requireFile rec {
    name = "Mathematica_${version}" + optionalString (lang != "en") "_${language}" + "_LINUX.sh";
    sha256 = "d34e02440d96f4f80804db08475aa3d5f22d7cb68ad37eafb3c8ea4ec0a268ba";
    message = ''
      This nix expression requires that ${name} is
      already part of the store. Find the file on your Mathematica CD
      and add it to the nix store with nix store add-file <FILE>.
    '';
  };
})
