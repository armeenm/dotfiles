{ lib, pkgs }:

let
  inherit (pkgs.stdenv) hostPlatform;

  version = "2.1.1";

  sources = {
    x86_64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-x64/-/cli-linux-x64-${version}.tgz";
      hash = "sha512-95RGRs0xMqzBuSj85pvXc7MQxokXHBBc3Ng1/I0EH83bq7sYrKKFw1FTXI7je6bZQjB+8q79bzUOoG1j0A192w==";
    };
    aarch64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-arm64/-/cli-linux-arm64-${version}.tgz";
      hash = "sha512-+v8jCh1HT6iH2uCIYTTaYv+xqE1aEWM0XOjnsi2/1jz6GringoxBTz1lmfUmPY+R570LSyEjc9LVW6DBrHbAyQ==";
    };
    aarch64-darwin = {
      url = "https://registry.npmjs.org/@opencode/cli-darwin-arm64/-/cli-darwin-arm64-${version}.tgz";
      hash = "sha512-Leq0p8zH5syoSLpEDo9rakmLzI6biLoDQYcjL0i+OIux4GRSs/1LBvZtcKdxpCLmhl7jUN1MdEL/7s6nRFj7GA==";
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
