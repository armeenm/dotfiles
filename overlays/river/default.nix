{ lib
, stdenv
, fetchFromGitHub
, zig
, wayland
, pkg-config
, scdoc
, xwayland
, wayland-protocols
, wlroots
, libxkbcommon
, pixman
, udev
, libevdev
, libinput
, libGL
, libX11
, xwaylandSupport ? true
}:

stdenv.mkDerivation rec {
  pname = "river";
  version = "unstable-2022-08-16";

  src = fetchFromGitHub {
    owner = "riverwm";
    repo = pname;
    rev = "844ffce03738d003a149ad9f022005f897194b2e";
    hash = "sha256-PkRoTFyPsKEvW4pFqRySJSvFGBMd0VFC22XDem8zzXo=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ zig wayland xwayland scdoc pkg-config ];

  buildInputs = [
    wayland-protocols
    wlroots
    libxkbcommon
    pixman
    udev
    libevdev
    libinput
    libGL
  ] ++ lib.optional xwaylandSupport libX11;

  dontConfigure = true;

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    runHook preInstall
    zig build -Drelease-safe -Dcpu=baseline ${lib.optionalString xwaylandSupport "-Dxwayland"} -Dman-pages --prefix $out install
    runHook postInstall
  '';

  /* Builder patch install dir into river to get default config
    When installFlags is removed, river becomes half broken.
    See https://github.com/riverwm/river/blob/7ffa2f4b9e7abf7d152134f555373c2b63ccfc1d/river/main.zig#L56
  */
  installFlags = [ "DESTDIR=$(out)" ];

  meta = with lib; {
    homepage = "https://github.com/ifreund/river";
    description = "A dynamic tiling wayland compositor";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
