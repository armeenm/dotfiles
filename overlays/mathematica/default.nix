{ pkgs
, lib
, stdenv
, autoPatchelfHook
, buildEnv
, makeWrapper
, requireFile
, R
, alsa-lib
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
, cudaSupport ? false
, nvidia_x11 ? null
, cudatoolkit ? null
, openjdk11
, pciutils
, tre
, xkeyboard_config
, xorg
, xz
, zlib
}:

let
  cudaDeps = lib.optional cudaSupport [
    nvidia_x11
    cudatoolkit
    cudatoolkit.lib
  ];
  
  cudaEnv = buildEnv {
    name = "mathematica-cuda";
    paths = cudaDeps;
    pathsToLink = [ "/bin" "/include" "/lib" ];
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

  buildInputs = old.buildInputs ++ cudaDeps ++ [
    alsa-lib
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
  ];

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];

  wrapProgramFlags = [
    "--prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [stdenv.cc.cc zlib libssh2]}"
    "--set USE_WOLFRAM_LD_LIBRARY_PATH 1"
    "--set QT_XKB_CONFIG_ROOT ${xkeyboard_config}/share/X11/xkb"
  ] ++ lib.optional cudaSupport [
    "--set CUDA_PATH ${cudaEnv}"
    "--set NVIDIA_DRIVER_LIBRARY_PATH ${cudaEnv}/lib/libnvidia-tls.so"
  ];

  installPhase = ''
    runHook preInstall

    cd $TMPDIR/Unix/Installer

    mkdir -p $out/usr/share $out/lib/udev/rules.d

    patchShebangs MathInstaller
    sed -i '
      s|^PATH=|# &|
      s|`hostname`|""|
      s|isRoot="false"|# &|
      s|^checkAvahiDaemon$|# &|
      s|/etc/udev/rules.d|$out/lib/udev/rules.d|
      s|/usr|$out/usr|
    ' MathInstaller

    XDG_DATA_HOME="$out/share" HOME="$out/home" vernierLink=y \
      ./MathInstaller -execdir="$out/bin" -targetdir="$out/libexec/Mathematica" -auto -createdir=y

    errLog="$out/libexec/Mathematica/InstallErrors"
    [ -f "$errLog" ] && echo "Installation errors:" && cat "$errLog" && rm "$errLog"

    for bin in $out/bin/*; do wrapProgram $bin ''${wrapProgramFlags[@]}; done

    runHook postInstall
  '';

  preFixup = "";

  dontPatchELF = false;
  autoPatchelfIgnoreMissingDeps = true;
})
