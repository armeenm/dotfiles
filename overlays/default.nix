final: prev: {
  river = prev.callPackage ./river { };

  cryptsetup = prev.cryptsetup.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  dconf = prev.dconf.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });
}
