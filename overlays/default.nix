final: prev: {
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  seafile-shared = prev.callPackage ./seafile-shared { pkgs = prev; };
  discord = prev.callPackage ./discord { pkgs = prev; };
  lightdm = prev.callPackage ./lightdm { pkgs = prev; };
}
