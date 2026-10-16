{ lib, pkgs }:

let
  inherit (pkgs.stdenv) hostPlatform;

  version = "2.0.32";

  sources = {
    x86_64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-x64/-/cli-linux-x64-${version}.tgz";
      hash = "sha512-Q50ofBQ3gdwjD6ZQhrVR8fVlm8D2xPMshy4dynY6l6b+qLdKgBSJ9WpoaDyrQX1YWmf2s7IN4RbXmoXXdJitDA==";
    };
    aarch64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-arm64/-/cli-linux-arm64-${version}.tgz";
      hash = "sha512-YR5O03irvmot/G0f8W49ehvhn5uqCFN3vf9JTsChvf47W6ldjKCNvsaCSXS2SP26dPOUMQBn7WqbKnyHzrNR5w==";
    };
    aarch64-darwin = {
      url = "https://registry.npmjs.org/@opencode/cli-darwin-arm64/-/cli-darwin-arm64-${version}.tgz";
      hash = "sha512-wASwXBsWaOAYcZrh4ZLl4oaVxPCUAGVb7zZvXBBF8TjWE4uWgY0D2tj6gn80JfcO3b/s9woM6dP/gKqYKK2EdQ==";
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
