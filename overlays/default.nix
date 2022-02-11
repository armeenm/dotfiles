final: prev: {
  discord-canary = prev.callPackage ./discord-canary { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  mdadm = prev.callPackage ./mdadm { pkgs = prev; };
  nixUnstable = prev.callPackage ./nixUnstable { pkgs = prev; };
  whatsapp-for-linux = prev.callPackage ./whatsapp-for-linux { pkgs = prev; };

  elfutils = prev.elfutils.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  ddclient = prev.ddclient.overrideAttrs (old: rec {
    src = prev.fetchFromGitHub {
      owner = "ddclient";
      repo = "ddclient";
      rev = "c29919bb0fb61eecbb7a9e02a8555fa9785f9b0a";
      hash = "sha256-nvuWyCJPe7yBxwDhUpNSevY+T3pFwA10xsjIG9y0/Aw=";
    };

    propagatedBuildInputs = old.buildInputs ++ (with prev.perlPackages; [
      TestMockModule
      TestTCP
      TestWarnings
      URI
    ]);

    nativeBuildInputs = [ prev.autoreconfHook ];

    preConfigure = "touch Makefile.PL";
    
    buildPhase = ''
      runHook preBuild

      mkdir -p $out/bin
      touch $out/bin/what5
      make -j $NIX_BUILD_CORES

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      perlFlags=$(head -n1 ddclient.in)
      perlFlags=''${perlFlags#* }
      sed -i "1s|$| $perlFlags|" ddclient

      make install

      runHook postInstall
    '';

    dontFixup = true;
    
    #fixupPhase = "";

    #postFixup = ''
    #  wrapProgram "$out/bin/ddclient" --prefix PERL5LIB : "${prev.perlPackages.makePerlPath propagatedBuildInputs}"
    #'';

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
