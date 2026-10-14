{ lib, pkgs }:

let
  inherit (pkgs.stdenv) hostPlatform;

  version = "2.0.29";

  sources = {
    x86_64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-x64/-/cli-linux-x64-${version}.tgz";
      hash = "sha512-pdG/T4uBxSC3hjUTOrTlx2pXlZl2VqABlb0AgDlZ8VO/q1pUUOYvooWmN5VeSi+EP8cKGVUsj0GUmMDeB2ympA==";
    };
    aarch64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-arm64/-/cli-linux-arm64-${version}.tgz";
      hash = "sha512-EsCnvp5lMJNem0YoQJM9PkXY0KrkiugL55M9J2JlImQzKdQnZ4CKutrPAXOnsa889qHhY5tjRpAnD/ZYFzhkRA==";
    };
    aarch64-darwin = {
      url = "https://registry.npmjs.org/@opencode/cli-darwin-arm64/-/cli-darwin-arm64-${version}.tgz";
      hash = "sha512-2tJOM20RpOjNeWsYcV3VfRFPDTeu1yMMbEqAQm1sEKmHX9087Sx/TxhwBlvtiudOGq6Kr3zyKmW1t7aFF6qCsQ==";
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
