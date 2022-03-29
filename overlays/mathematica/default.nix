{ pkgs
, lib
, stdenv
, autoPatchelfHook
, buildEnv
, makeWrapper
, requireFile
, alsa-lib
, cups
, dbus
, flite
, fontconfig
, freetype
, glib
, gmpxx
, keyutils
, libGL
, libGLU
, libpcap
, libssh2
, libtins
, libuuid
, libxkbcommon
, libxml2
, llvmPackages_12
, matio
, mpfr
, ncurses
, opencv4
, openjdk11
, openssl
, pciutils
, tre
, unixODBC
, xkeyboard_config
, xorg
, zlib
, installer ? "Mathematica_13.0.0_LINUX.sh"
, cudaSupport ? false
, nvidia_x11 ? null
, cudatoolkit ? null
}:

let
  cudaDeps = [
    (nvidia_x11.override { libsOnly = true; })
    cudatoolkit
    cudatoolkit.lib
  ];

  cudaEnv = buildEnv {
    name = "mathematica-cuda";
    paths = cudaDeps;
    pathsToLink = [ "/bin" "/include" "/lib" ];
    postBuild = "ln -s $out/lib $out/lib64";
  };

in
stdenv.mkDerivation rec {

  version = "13.0.0";
  name = "mathematica-${version}";

  src = requireFile rec {
    name = installer;
    sha256 = "15bbad39a5995031325d1d178f63b00e71706d3ec9001eba6d1681fbc991d3e1";
    message = ''
      This nix expression requires that ${name} is
      already part of the store. Find the file on your Mathematica CD
      and add it to the nix store with nix store add-file <FILE>.
    '';
  };

  buildInputs = [
    alsa-lib
    cups.lib
    dbus
    flite
    fontconfig
    freetype
    glib
    gmpxx
    keyutils.lib
    libGL
    libGLU
    libpcap
    libtins
    libuuid
    libxkbcommon
    libxml2
    llvmPackages_12.libllvm.lib
    matio
    mpfr
    ncurses
    opencv4
    openjdk11
    openssl
    pciutils
    tre
    unixODBC
    xkeyboard_config
  ] ++ (with xorg; [
    libICE
    libSM
    libX11
    libXScrnSaver
    libXcomposite
    libXcursor
    libXdamage
    libXext
    libXfixes
    libXi
    libXinerama
    libXmu
    libXrandr
    libXrender
    libXtst
    libxcb
  ]) ++ lib.optionals cudaSupport cudaDeps;

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];

  wrapProgramFlags = [
    "--prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [stdenv.cc.cc zlib libssh2]}"
    "--set QT_XKB_CONFIG_ROOT ${xkeyboard_config}/share/X11/xkb"
    "--set USE_WOLFRAM_LD_LIBRARY_PATH 1"
  ] ++ lib.optionals cudaSupport [
    "--set CUDA_PATH ${cudaEnv}"
    "--set NVIDIA_DRIVER_LIBRARY_PATH ${cudaEnv}/lib/libnvidia-tls.so"
  ];

  unpackPhase = ''
    runHook preUnpack

    offset=$(${stdenv.shell} -c "$(grep -axm1 -e 'offset=.*' $src); echo \$((\$offset + 1))" $src)
    tail -c +$offset $src | tar -xf -

    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    cd "$TMPDIR/Unix/Installer"

    mkdir -p "$out/lib/udev/rules.d"

    patchShebangs MathInstaller
    sed -i '
      s|^PATH=|# &|
      s|isRoot="false"|# &|
      s|^checkAvahiDaemon$|# &|
      s|`hostname`|""|
      s|/etc/udev/rules.d|$out/lib/udev/rules.d|
    ' MathInstaller

    XDG_DATA_HOME="$out/share" HOME="$TMPDIR/home" vernierLink=y \
      ./MathInstaller -execdir="$out/bin" -targetdir="$out/libexec/Mathematica" -auto -verbose -createdir=y

    errLog="$out/libexec/Mathematica/InstallErrors"
    if [ -f "$errLog" ]; then
      echo "Installation errors:"
      cat "$errLog"
      return 1
    fi

    for bin in $out/bin/*; do wrapProgram "$bin" ''${wrapProgramFlags[@]}; done

    runHook postInstall
  '';

  autoPatchelfIgnoreMissingDeps = true;
  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  preferLocalBuild = true;
}
