{ lib, pkgs }:

let
  inherit (pkgs.stdenv) hostPlatform;

  version = "0.1.5";
  pname = "opencode-browse";

  # The npm package's launcher only locates the platform's native binary and
  # execs it, so both halves come from the published release, like
  # home/opencode.nix. Pinning the release keeps the CLI on the same version as
  # the Chrome Web Store extension, which updates on its own.
  platform =
    {
      x86_64-linux = {
        name = "linux-x64";
        hash = "sha256-i6c/FjIqQlRMrTqXSQv9FJV7l7EItY9DkT5soIijzdc=";
      };
      aarch64-linux = {
        name = "linux-arm64";
        hash = "sha256-C1UOi2YjmUqUe8mYcEtLxeeW4wcyPeX5R81KvXx9fas=";
      };
      x86_64-darwin = {
        name = "darwin-x64";
        hash = "sha256-/PiaQKnbNIYY7cqRT+hLooS0jH7PWCmeF0gFYHJjtYk=";
      };
      aarch64-darwin = {
        name = "darwin-arm64";
        hash = "sha256-1jsw2bidBQcKMxDS2AQ14Gq5zDUha4AazGj5aeFCAr8=";
      };
    }.${hostPlatform.system}
      or (throw "browse is unsupported on ${hostPlatform.system}");

  # Only the lockfile comes from the source tree: the launcher leaves its
  # dependencies (ws, acorn, pngjs, pixelmatch, patchright-core) external
  # instead of bundling them, and the published tarball omits them.
  src = pkgs.fetchFromGitHub {
    owner = "anomalyco";
    repo = "browse";
    rev = "v${version}";
    hash = "sha256-BIzCe/atioMcWg+UvuXeEpijh6CHIXe5tPpR5L0JYcU=";
  };

  dist = pkgs.fetchurl {
    url = "https://registry.npmjs.org/@opencode/browse/-/browse-${version}.tgz";
    hash = "sha256-BFFS3Y45k+yepd40ZaX69Nw1M/vH0ueoiBO2LwQzGwM=";
  };

  native = pkgs.fetchurl {
    url = "https://registry.npmjs.org/@opencode/browse-${platform.name}/-/browse-${platform.name}-${version}.tgz";
    hash = platform.hash;
  };

  nodeModules = pkgs.stdenvNoCC.mkDerivation {
    pname = "${pname}-node-modules";
    inherit version src;

    nativeBuildInputs = [ pkgs.bun ];

    buildPhase = ''
      runHook preBuild
      export HOME=$TMPDIR
      cd cli
      bun install --frozen-lockfile --production
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      cd "$NIX_BUILD_TOP/$sourceRoot"
      mkdir -p $out
      cp -r cli/node_modules $out/
      runHook postInstall
    '';

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-1ZtjRBkXx6ihf5PJC4Ffn6TyzzjwoRJbW3mlwWD1ne0=";

    # Fixup would rewrite shebangs in node_modules/.bin to the build sandbox's
    # bash, and a fixed-output derivation may not reference store paths.
    dontFixup = true;
  };
in
pkgs.stdenvNoCC.mkDerivation {
  inherit pname version;

  src = dist;

  nativeBuildInputs = [ pkgs.makeWrapper ] ++ lib.optionals hostPlatform.isLinux [ pkgs.patchelf ];

  unpackPhase = ''
    mkdir dist native
    tar -xzf ${dist} -C dist
    tar -xzf ${native} -C native
  '';

  installPhase = ''
    mkdir -p $out/lib/browse/node_modules/@opencode/browse-${platform.name}
    cp -r dist/package/. $out/lib/browse/
    cp -r native/package/. $out/lib/browse/node_modules/@opencode/browse-${platform.name}/
    cp -r ${nodeModules}/node_modules/. $out/lib/browse/node_modules/

    mkdir -p $out/bin
    # The relay's self-update would `npm install -g` into the read-only store;
    # updates come from bumping this derivation instead.
    makeWrapper ${lib.getExe pkgs.nodejs_22} $out/bin/browse \
      --add-flags "$out/lib/browse/dist/cli.mjs" \
      --set-default OPENCODE_BROWSER_AUTO_UPDATE off
  '';

  # The platform tarball is built on a generic Linux and carries the usual
  # /lib64 interpreter, which NixOS does not have.
  postFixup = lib.optionalString hostPlatform.isLinux ''
    patchelf --set-interpreter ${pkgs.stdenv.cc.bintools.dynamicLinker} \
      $out/lib/browse/node_modules/@opencode/browse-${platform.name}/bin/browse
  '';

  meta = {
    description = "Let AI agents use your real browser: MCP server for the browse extension";
    homepage = "https://github.com/anomalyco/browse";
    license = lib.licenses.mit;
    mainProgram = "browse";
    platforms = lib.platforms.unix;
  };
}
