{ pkgs, lib, stdenv, ... }:

with lib;

pkgs.mathematica.overrideAttrs (old: rec {
  version = "13.0.0";
  lang = "en";
  language = "English";
  name = "mathematica-${version}" + optionalString (lang != "en") "-${lang}";

  src = pkgs.requireFile rec {
    name = "Mathematica_${version}" + optionalString (lang != "en") "_${language}" + "_LINUX.sh";
    sha256 = "d34e02440d96f4f80804db08475aa3d5f22d7cb68ad37eafb3c8ea4ec0a268ba";
    message = ''
              This nix expression requires that ${name} is
              already part of the store. Find the file on your Mathematica CD
              and add it to the nix store with nix store add-file <FILE>.
            '';
  };

  buildInputs = with pkgs; old.buildInputs ++ [
    R
    alsa-lib
    autoPatchelfHook
    cudaPackages.cudatoolkit_11_5
    cups.lib
    flite
    gmpxx
    keyutils.lib
    libpcap
    libtins
    libxkbcommon
    llvmPackages_12.libllvm.lib
    matio
    mpfr
    openjdk11
    pciutils
    tre
    xorg.libXScrnSaver
    xorg.libXcomposite
    xorg.libXdamage
    xorg.libXinerama
    xz
  ];

  ldpath = makeLibraryPath buildInputs
           + optionalString (stdenv.hostPlatform.system == "x86_64-linux")
             (":" + makeSearchPathOutput "lib" "lib64" buildInputs);

  preFixup = "";

  dontPatchELF = false;
  autoPatchelfIgnoreMissingDeps = true;
})
