{ lib, pkgs }:

let
  inherit (pkgs.stdenv) hostPlatform;

  version = "2.2.2";

  sources = {
    x86_64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-x64/-/cli-linux-x64-${version}.tgz";
      hash = "sha512-wa0ol1HUaKNDTqjfqJwqmq3n4tlejBy2UT48wVRCypRUGy9WbIpXeL9xlHmljAodQD6HcZR2dqiJYFTlDMqIVA==";
    };
    aarch64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-arm64/-/cli-linux-arm64-${version}.tgz";
      hash = "sha512-cyp6l1SLFGV7NxXd5FgBIJT8V5mQhBjs+F9vh4pRSam9kRGmv/nUEvgPMXKfNrZdsllTlLoPvzgHWzQZz2ff1A==";
    };
    aarch64-darwin = {
      url = "https://registry.npmjs.org/@opencode/cli-darwin-arm64/-/cli-darwin-arm64-${version}.tgz";
      hash = "sha512-cH6cZvu4OuuB926Jw4QBE6F6jyzlMr7d5TTgRZw8iON7XQ3DyyQG3bS6yHo+yHwp+rKbwGoygnpua6YIIIZV9w==";
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
