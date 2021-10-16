final: prev: {
  # TODO Make cleaner
  nixUnstable = prev.callPackage ./nixUnstable { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  seafile-shared = prev.callPackage ./seafile-shared { pkgs = prev; };
  discord = prev.callPackage ./discord { pkgs = prev; };
  lightdm = prev.callPackage ./lightdm { pkgs = prev; };
  whatsapp-for-linux = prev.callPackage ./whatsapp-for-linux { pkgs = prev; };

  #linux_latest_hardened = prev.linuxPackagesFor (prev.

  # FIXME
  linuxPackages_latest = prev.linuxPackages_latest.extend (kfinal: kprev: {
    nvidia_x11 = kprev.nvidia_x11.overrideAttrs (old: {
      passthru.settings = old.passthru.settings.overrideAttrs (old: {
        makeFlags = old.makeFlags ++ [ ''CPPFLAGS=-DDEFAULT_RC_FILE=~/.config/nvidia-settings-rc'' ];
      });
    });
  });
}
