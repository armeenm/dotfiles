final: prev: {
  discord-canary = prev.callPackage ./discord-canary { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };

  elfutils = prev.elfutils.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  ghc = prev.ghc.overrideAttrs (_: {
    __contentAddressed = false;
  });

  gcc = prev.gcc.overrideAttrs (_: {
    __contentAddressed = false;
  });

  gcc-wrapper = prev.gcc-wrapper.overrideAttrs (_: {
    __contentAddressed = false;
  });

  gfortran = prev.gfortran.overrideAttrs (_: {
    __contentAddressed = false;
  });
}
