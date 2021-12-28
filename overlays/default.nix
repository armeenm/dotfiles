final: prev: {
  discord = prev.callPackage ./discord { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  mdadm = prev.callPackage ./mdadm { pkgs = prev; };
  nixUnstable = prev.callPackage ./nixUnstable { pkgs = prev; };
  profanity = prev.callPackage ./profanity { pkgs = prev; };
  seafile-shared = prev.callPackage ./seafile-shared { pkgs = prev; };
  swtpm = prev.callPackage ./swtpm { pkgs = prev; };
  whatsapp-for-linux = prev.callPackage ./whatsapp-for-linux { pkgs = prev; };

  elfutils = prev.elfutils.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  go = prev.go.overrideAttrs (_: {
    doCheck = false;
    checkPhase = "";
  });
}
