final: prev: {
  #discord-canary = prev.callPackage ./discord-canary { pkgs = prev; };
  #mathematica = prev.callPackage ./mathematica { pkgs = prev; };

  cryptsetup = prev.cryptsetup.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  dconf = prev.dconf.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });
}
