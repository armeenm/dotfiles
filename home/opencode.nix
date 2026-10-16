{ lib, pkgs }:

let
  inherit (pkgs.stdenv) hostPlatform;

  version = "2.0.30";

  sources = {
    x86_64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-x64/-/cli-linux-x64-${version}.tgz";
      hash = "sha512-oBw8dtg9BWiKkhyPOxA6GN+o78zmQfjgVi7Hz9de1pJZJVyGBMLlkYk1EwPeS+vWyOCSaty2FxCAsYH1bMdS2g==";
    };
    aarch64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-arm64/-/cli-linux-arm64-${version}.tgz";
      hash = "sha512-EQzVA40jWkbYMQGdqC3y7I7DOUnw5zYhPPwCa2vFb/MK/BuBkHZqPcsDABD82ECaNVGJo43kz8s0Ckodb8EA4w==";
    };
    aarch64-darwin = {
      url = "https://registry.npmjs.org/@opencode/cli-darwin-arm64/-/cli-darwin-arm64-${version}.tgz";
      hash = "sha512-pXiRPtYr8BERCcx4/dUzCUra3vxsQffTa/8l6rjODNBgWAoxLlWJaXxrGmQ907uRPaUY0OT7HQvTmUzRTnkj9g==";
    };
  };

  source = sources.${hostPlatform.system}
    or (throw "OpenCode 2 is unsupported on ${hostPlatform.system}");
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "opencode";
  inherit version;

  src = pkgs.fetchurl source;

  nativeBuildInputs = lib.optionals hostPlatform.isLinux [ pkgs.patchelf ];

  unpackPhase = ''
    tar -xzf $src
  '';

  installPhase = ''
    install -Dm755 package/bin/opencode $out/bin/opencode
    mkdir -p $out/share
  '';

  postFixup = lib.optionalString hostPlatform.isLinux ''
    patchelf --set-interpreter ${pkgs.stdenv.cc.bintools.dynamicLinker} $out/bin/opencode
  '';

  meta = {
    description = "OpenCode AI coding agent";
    homepage = "https://opencode.ai";
    license = lib.licenses.mit;
    mainProgram = "opencode";
    platforms = builtins.attrNames sources;
  };
}
