final: prev: {
  discord-canary = prev.callPackage ./discord-canary { pkgs = prev; };
  mathematica = prev.callPackage ./mathematica { pkgs = prev; };
  nixUnstable = prev.callPackage ./nixUnstable { pkgs = prev; };

  elfutils = prev.elfutils.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  #libuv = prev.libuv.overrideAttrs (_: {
  #  doCheck = false;
  #});

  gnutls = prev.gnutls.overrideAttrs (_: {
    doCheck = false;
  });

  wlroots = prev.wlroots.overrideAttrs (_: rec {
    version = "0.15.1";
    src = prev.fetchFromGitLab {
      domain = "gitlab.freedesktop.org";
      owner = "wlroots";
      repo = "wlroots";
      rev = version;
      hash = "sha256-MFR38UuB/wW7J9ODDUOfgTzKLse0SSMIRYTpEaEdRwM=";
    };
  });

  grim = prev.grim.overrideAttrs (_: {
    version = "master";
    src = prev.fetchFromGitHub {
      owner = "emersion";
      repo = "grim";
      rev = "20c7c47a0aac09371c570c060f5f52f7e165e67a";
      hash = "sha256-93QlKnj3/l3rTo9Ta7NqjKlPW6x43zZUBs01PaRXgig=";
    };
  });
}
