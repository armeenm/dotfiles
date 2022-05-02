final: prev: {
  discord-canary = prev.callPackage ./discord-canary { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };

  elfutils = prev.elfutils.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  libpulseaudio = prev.libpulseaudio.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  pulseaudio = prev.pulseaudio.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  #wlroots = prev.wlroots.overrideAttrs (old: {
  #  buildInputs = old.buildInputs ++ [ prev.vulkan-headers ];
  #});
}
