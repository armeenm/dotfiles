{ inputs, ... }:

final: prev: {
  mathematica = prev.mathematica.overrideAttrs (_: {
    postInstall = ''
      ln -s "$out/libexec/Mathematica/Executables/wolframscript" "$out/bin/wolframscript"
    '';
  });

  pcsclite = prev.pcsclite.overrideAttrs (old: {
    postPatch = old.postPatch + ''
      substituteInPlace src/libredirect.c src/spy/libpcscspy.c \
        --replace-fail "libpcsclite_real.so.1" "$lib/lib/libpcsclite_real.so.1"
    '';
  });
}
