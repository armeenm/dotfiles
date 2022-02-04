final: prev: {
  discord-ptb = prev.callPackage ./discord-ptb { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  mdadm = prev.callPackage ./mdadm { pkgs = prev; };
  nixUnstable = prev.callPackage ./nixUnstable { pkgs = prev; };
  whatsapp-for-linux = prev.callPackage ./whatsapp-for-linux { pkgs = prev; };

  elfutils = prev.elfutils.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  go = prev.go.overrideAttrs (_: {
    doCheck = false;
  });

  python39Packages.aniso8601 = prev.python39Packages.aniso8601.overrideAttrs (_: {
    __contentAddressed = false;
  });

  python39Packages.websockets = prev.python39Packages.websockets.overrideAttrs (_: {
    doInstallCheck = false;
  });

  python39Packages.fakeredis = prev.python39Packages.websockets.overrideAttrs (_: {
    doInstallCheck = false;
  });

  git = prev.git.overrideAttrs (_: {
    doInstallCheck = false;
  });

  fd = prev.fd.overrideAttrs (_: {
    doCheck = false;
  });

  gfortran = prev.gfortran.overrideAttrs (_: {
    doCheck = false;
  });

  gdb = prev.gdb.overrideAttrs (old: {
    NIX_CFLAGS_COMPILE = old.NIX_CFLAGS_COMPILE + " -Wno-error";
  });

  ddclient = prev.ddclient.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      owner = "ddclient";
      repo = "ddclient";
      rev = "c29919bb0fb61eecbb7a9e02a8555fa9785f9b0a";
      hash = "sha256-nvuWyCJPe7yBxwDhUpNSevY+T3pFwA10xsjIG9y0/Aw=";
    };

    nativeBuildInputs = [ prev.autoreconfHook ];

    preConfigure = ''
      touch Makefile.PL
      substituteInPlace ddclient.in \
        --replace 'in the output of ifconfig' 'in the output of ip addr show' \
        --replace 'ifconfig -a' '${prev.iproute2}/sbin/ip addr show' \
        --replace 'ifconfig $arg' '${prev.iproute2}/sbin/ip addr show $arg' \
        --replace '/usr/bin/perl' '${prev.perl}/bin/perl' # Until we get the patchShebangs fixed (issue #55786) we need to patch this manually
    '';

    installPhase = ''
      make install
    '';

    doCheck = false;

    checkPhase = "make VERBOSE=1 check";
  });

  wlroots = prev.wlroots.overrideAttrs (_: rec {
    version = "0.15.1";
    src = prev.fetchFromGitLab {
      domain = "gitlab.freedesktop.org";
      owner = "wlroots";
      repo = "wlroots";
      rev = version;
      hash = "sha256-MFR38UuB/wW7J9ODDUOfgTzKLse0SSMIRYTpEaEdRwM=";
    };
  });

  grim = prev.grim.overrideAttrs (_: {
    version = "master";
    src = prev.fetchFromGitHub {
      owner = "emersion";
      repo = "grim";
      rev = "20c7c47a0aac09371c570c060f5f52f7e165e67a";
      hash = "sha256-93QlKnj3/l3rTo9Ta7NqjKlPW6x43zZUBs01PaRXgig=";
    };
  });
}
