{ lib, pkgs }:

let
  inherit (pkgs.stdenv) hostPlatform;

  version = "2.0.28";

  sources = {
    x86_64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-x64/-/cli-linux-x64-${version}.tgz";
      hash = "sha512-HdgQ2c9FrER88w3Alm/UGdzU55lCc0FXxSm+VNMXUz+Yyv03uLc0Z+PxSnEBysmYNKcFvcnxdT7e0xGPoytrNg==";
    };
    aarch64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-arm64/-/cli-linux-arm64-${version}.tgz";
      hash = "sha512-BOjXZMtvYiQ9xowfHXJseARn7wHN8RTvRvYvMMIsjIOEJOYtv4aPvB4l0BmmnhUEhZQgrprPFamkUDy0lDiYiw==";
    };
    aarch64-darwin = {
      url = "https://registry.npmjs.org/@opencode/cli-darwin-arm64/-/cli-darwin-arm64-${version}.tgz";
      hash = "sha512-HYFeBK8drYVq9b9DlQ51pBLCaL+GNyih1ysTQfmekW7YhAJngPYDM7xMgCINXopRVMGTw7EmbJPrMm+9V0G7Yg==";
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
