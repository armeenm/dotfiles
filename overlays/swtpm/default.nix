{ pkgs, fetchFromGitHub, gnutls, python3, which, ... }:

pkgs.swtpm.overrideAttrs (old: rec {
  version = "0.7.0";
  src = fetchFromGitHub {
    owner = "stefanberger";
    repo = "swtpm";
    rev = "v${version}";
    sha256 = "5MKQmZxTW8WofmTkV9kGeGN5RxsgVVMFZEF3rPDUO6Q=";
  };

  patches = [];

  checkInputs = [
    python3 which
  ];

  postPatch = ''
    patchShebangs tests/*
    # Makefile tries to create the directory /var/lib/swtpm-localca, which fails
    substituteInPlace samples/Makefile.am \
        --replace 'install-data-local:' 'do-not-execute:'
    # Use the correct path to the certtool binary
    # instead of relying on it being in the environment
    substituteInPlace src/swtpm_localca/swtpm_localca.c --replace \
        '# define CERTTOOL_NAME "certtool"' \
        '# define CERTTOOL_NAME "${gnutls}/bin/certtool"'
  '';
  
  doCheck = true;
})
