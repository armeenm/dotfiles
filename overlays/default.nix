final: prev: {
  discord-canary = prev.callPackage ./discord-canary { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  nixUnstable = prev.callPackage ./nixUnstable { pkgs = prev; };

  elfutils = prev.elfutils.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });
}
