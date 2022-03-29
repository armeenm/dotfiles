{ lib, stdenv, fetchFromGitHub, kernel }:

stdenv.mkDerivation rec {
  name = "lkrg";
  version = "0.9.2";

  src = fetchFromGitHub {
    owner = "lkrg-org";
    repo = "lkrg";
    rev = version;
    hash = "";
  };

  hardeningDisable = [ "pic" ];
  nativeBuildInputs = kernel.moduleBuildDependencies;

  makeFlags = [
    "KERNELRELEASE=${kernel.modDirVersion}"
    "KERNEL=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    #"INSTALL_MOD_PATH=$(out)"
  ];
};
