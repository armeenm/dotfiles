final: prev: {
  river = prev.callPackage ./river { };

  cryptsetup = prev.cryptsetup.overrideAttrs (_: {
    doCheck = false;
  });

  mathematica = prev.mathematica.overrideAttrs (_: {
    postInstall = ''
      ln -s "$out/libexec/Mathematica/Executables/wolframscript" "$out/bin/wolframscript"
    '';
  });
}
