final: prev: {
  cryptsetup = prev.cryptsetup.overrideAttrs (_: {
    doCheck = false;
  });

  girara = prev.girara.overrideAttrs (_: {
    doCheck = false;
  });

  mathematica = prev.mathematica.overrideAttrs (_: {
    postInstall = ''
      ln -s "$out/libexec/Mathematica/Executables/wolframscript" "$out/bin/wolframscript"
    '';
  });
}
