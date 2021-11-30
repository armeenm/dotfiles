final: prev: {
  # TODO Make cleaner
  discord = prev.callPackage ./discord { pkgs = prev; };
  lightdm = prev.callPackage ./lightdm { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  mdadm = prev.callPackage ./mdadm { pkgs = prev; };
  nixUnstable = prev.callPackage ./nixUnstable { pkgs = prev; };
  seafile-shared = prev.callPackage ./seafile-shared { pkgs = prev; };
  whatsapp-for-linux = prev.callPackage ./whatsapp-for-linux { pkgs = prev; };
  swtpm = prev.callPackage ./swtpm { pkgs = prev; };
  profanity = prev.callPackage ./profanity { pkgs = prev; };

  # FIXME
  linuxPackages_latest = prev.linuxPackages_latest.extend (kfinal: kprev: {
    nvidia_x11 = kprev.nvidia_x11.overrideAttrs (old: {
      passthru.settings = old.passthru.settings.overrideAttrs (old: {
        makeFlags = old.makeFlags ++ [ ''CPPFLAGS=-DDEFAULT_RC_FILE=~/.config/nvidia-settings-rc'' ];
      });
    });
  });
}
