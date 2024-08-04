{ inputs, ... }:

final: prev: {
  mathematica = prev.mathematica.overrideAttrs (_: {
    postInstall = ''
      ln -s "$out/libexec/Mathematica/Executables/wolframscript" "$out/bin/wolframscript"
    '';
  });

  /*
  ncmpcpp = prev.ncmpcpp.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      owner = "ncmpcpp";
      repo = "ncmpcpp";
      rev = "81e5cf58b44be4ec0dc50722e2ed6d534df3973d";
      hash = "sha256-fSy5CMrVhU48iu7H4fxXtNrxXHMtH1k0gizk00z7CgA=";
    };
  });
*/
}
