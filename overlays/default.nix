final: prev: {
  discord = prev.callPackage ./discord { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  mdadm = prev.callPackage ./mdadm { pkgs = prev; };
  nixUnstable = prev.callPackage ./nixUnstable { pkgs = prev; };
  profanity = prev.callPackage ./profanity { pkgs = prev; };
  swtpm = prev.callPackage ./swtpm { pkgs = prev; };
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

  git = prev.git.overrideAttrs (_: {
    doInstallCheck = false;
  });

  fd = prev.fd.overrideAttrs (_: {
    doCheck = false;
  });
}
