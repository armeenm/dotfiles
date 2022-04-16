final: prev: {
  discord-canary = prev.callPackage ./discord-canary { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };

  pythonPackages = prev.pythonPackages.override {
    packageOverride = pfinal: pprev: {
      websockets = pprev.websockets.overrideAttrs (_: {
        doInstallCheck = false;
      });

      pyclip = pprev.pyclip.overrideAttrs (_: {
        doInstallCheck = false;
      });
    };
  };

  llvmPackages_11 = prev.llvmPackages_11 // {
    llvm = prev.llvmPackages_11.llvm.overrideAttrs (_: {
      doCheck = false;
    });
  };

  fish = prev.fish.overrideAttrs (_: {
    doCheck = false;
  });

  libuv = prev.libuv.overrideAttrs (_: {
    doCheck = false;
  });

  elfutils = prev.elfutils.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  ghc = prev.ghc.overrideAttrs (_: {
    __contentAddressed = false;
  });

  ghc-musl = prev.ghc.overrideAttrs (_: {
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
