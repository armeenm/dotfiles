{ pkgs
, lib
, stdenv
, autoPatchelfHook
, buildEnv
, makeWrapper
, requireFile
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
, nvidia_x11 ? pkgs.linuxKernel.packages.linux_5_15_hardened.nvidiaPackages.stable
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
      (nvidia_x11.override { libsOnly = true; })
      cudaPackages.cudatoolkit_11_5
      cudaPackages.cudatoolkit_11_5.lib
    ];
    pathsToLink = [ "/lib" "/bin" "/include" ];
    postBuild = "ln -s $out/lib $out/lib64";
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

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];

  wrapProgramFlags = [
    "--prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [stdenv.cc.cc zlib libssh2]}"
    "--set USE_WOLFRAM_LD_LIBRARY_PATH 1"
    "--set QT_XKB_CONFIG_ROOT ${xkeyboard_config}/share/X11/xkb"
    "--set CUDA_PATH ${cuda}"
    "--set NVIDIA_DRIVER_LIBRARY_PATH ${cuda}/lib/libnvidia-tls.so"
  ];

  installPhase = ''
    runHook preInstall

    cd $TMPDIR/Unix/Installer

    # Fix MathInstaller shebang and PATH
    patchShebangs MathInstaller
    sed -i '
      s/^PATH=/# &/
      s/^checkSELinux_$/# &/
      s/^checkAvahiDaemon$/# &/
      s/^setHome$/# &/
    ' MathInstaller

    XDG_DATA_HOME="$out/share" DEBUG=true wolframScript=n \
      ./MathInstaller -auto -createdir=y -execdir="$out/bin" -targetdir="$out/libexec/Mathematica"

    errLog="$out/libexec/Mathematica/InstallErrors"
    [ -f "$errLog" ] && cat "$errLog" && rm "$errLog"

    for bin in $out/bin/*; do wrapProgram $bin ''${wrapProgramFlags[@]}; done

    runHook postInstall
  '';

  preFixup = "";

  dontPatchELF = false;
  autoPatchelfIgnoreMissingDeps = true;
})
