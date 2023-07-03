{ inputs, ... }:

final: prev: {
  mathematica = prev.mathematica.overrideAttrs (_: {
    postInstall = ''
      ln -s "$out/libexec/Mathematica/Executables/wolframscript" "$out/bin/wolframscript"
    '';
  });

  waybar = prev.waybar.overrideAttrs (old: {
    mesonFlags = old.mesonFlags ++ [ "-Dexperimental=true" ];
  });

  openvpn3 = inputs.nixpkgs-old.legacyPackages.x86_64-linux.callPackage ./openvpn3 { };
}
