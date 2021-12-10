{ pkgs
, lib
, stdenv
, buildEnv
, requireFile
, autoPatchelfHook
, R
, alsa-lib
, bash
, cudaPackages
, cups
, flite
, gmpxx
, keyutils
, libpcap
, libssh2
, libtins
, libxkbcommon
, llvmPackages_12
, matio
, mpfr
, nvidia_x11 ? pkgs.linuxPackages_5_15_hardened.nvidia_x11_beta
, openjdk11
, pciutils
, tre
, xkeyboard_config
, xorg
, xz
, zlib
}:

let
  cuda = buildEnv {
    name = "mathematica-cuda";
    paths = [
      nvidia_x11
      cudaPackages.cudatoolkit_11_5
      cudaPackages.cudatoolkit_11_5.lib
    ];
    pathsToLink = [ "/lib" "/bin" ];
  };

in pkgs.mathematica.overrideAttrs (old: rec {

  version = "13.0.0";
  lang = "en";
  language = "English";
  name = "mathematica-${version}" + lib.optionalString (lang != "en") "-${lang}";

  src = requireFile rec {
    name = "Mathematica_${version}" + lib.optionalString (lang != "en") "_${language}" + "_LINUX.sh";
    sha256 = "d34e02440d96f4f80804db08475aa3d5f22d7cb68ad37eafb3c8ea4ec0a268ba";
    message = ''
              This nix expression requires that ${name} is
              already part of the store. Find the file on your Mathematica CD
              and add it to the nix store with nix store add-file <FILE>.
            '';
  };

  buildInputs = old.buildInputs ++ [
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
    nvidia_x11
    openjdk11
    pciutils
    tre
    xorg.libXScrnSaver
    xorg.libXcomposite
    xorg.libXdamage
    xorg.libXinerama
    xz
  ];

  ldpath = lib.makeLibraryPath buildInputs
           + lib.optionalString (stdenv.hostPlatform.system == "x86_64-linux")
             (":" + lib.makeSearchPathOutput "lib" "lib64" buildInputs);

  installPhase = ''
    cd Installer

    # don't restrict PATH, that has already been done
    sed -i -e 's/^PATH=/# PATH=/' MathInstaller

    # Fix the installation script as follows:
    # 1. Adjust the shebang
    # 2. Use the wrapper in the desktop items
    substituteInPlace MathInstaller \
      --replace "/bin/bash" "${pkgs.bash}/bin/bash" \
      --replace "Executables/Mathematica" "../../bin/mathematica"

    # Install the desktop items
    export XDG_DATA_HOME="$out/share"

    echo "=== Running MathInstaller ==="
    ./MathInstaller -auto -createdir=y -execdir=$out/bin -targetdir=$out/libexec/Mathematica -silent

    # Fix library paths
    cd $out/libexec/Mathematica/Executables
    for path in mathematica MathKernel Mathematica WolframKernel wolfram math; do
      sed -i -e "2iexport LD_LIBRARY_PATH=${zlib}/lib:${stdenv.cc.cc.lib}/lib:${libssh2}/lib:\''${LD_LIBRARY_PATH}\n" $path
    done

    # Fix xkeyboard config path for Qt
    for path in mathematica Mathematica; do
      sed -i -e "2iexport QT_XKB_CONFIG_ROOT=\"${xkeyboard_config}/share/X11/xkb\"\n" $path
    done

    # Remove some broken libraries
    rm -f $out/libexec/Mathematica/SystemFiles/Libraries/Linux-x86-64/libz.so*

    # Set environment variable to fix libQt errors - see https://github.com/NixOS/nixpkgs/issues/96490
    wrapProgram $out/bin/mathematica \
      --set USE_WOLFRAM_LD_LIBRARY_PATH 1 \
      --set CUDA_PATH ${cuda}
  '';

  preFixup = "";

  dontPatchELF = false;
  autoPatchelfIgnoreMissingDeps = true;
})
