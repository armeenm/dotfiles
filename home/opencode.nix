# OpenCode is built from source rather than from the published
# @opencode/cli-linux-x64 tarball: the released binaries embed compiled Bun
# bytecode, so a local change to the MCP OAuth flow (see
# opencode-google-oauth.patch) cannot be applied to them. This follows the
# build recipe the OpenCode repository ships in nix/opencode.nix; keep the
# pinned hashes below in sync when bumping `version`.
{ lib, pkgs }:

let
  inherit (pkgs.stdenv) hostPlatform;

  version = "2.1.1";

  src = pkgs.fetchFromGitHub {
    owner = "anomalyco";
    repo = "opencode";
    tag = "v${version}";
    hash = "sha256-CoujRYCvqFXDJ6bzMxinT8CSPyKiLSAzCINjVaZBm/Y=";
  };

  # `bun install` only needs the workspace manifests and the scripts it runs;
  # keeping the tree small also keeps the fixed-output hash stable. Mirrors the
  # fileset in the upstream nix/node_modules.nix.
  source = lib.cleanSourceWith {
    src = src;
    filter = path: _type:
      let
        relative = lib.removePrefix (toString src) (toString path);
        roots = [
          "/packages"
          "/services"
          "/bun.lock"
          "/package.json"
          "/patches"
          "/install"
          "/.github"
          "/.github/TEAM_MEMBERS"
          "/script"
        ];
      in
      relative == ""
      || lib.any (root: relative == root || lib.hasPrefix "${root}/" relative) roots;
  };

  bunCpu = if hostPlatform.isAarch64 then "arm64" else "x64";
  bunOs = if hostPlatform.isLinux then "linux" else "darwin";

  nodeModules = pkgs.stdenvNoCC.mkDerivation {
    pname = "opencode-node_modules";
    inherit version;
    src = source;

    impureEnvVars = lib.fetchers.proxyImpureEnvVars ++ [
      "GIT_PROXY_COMMAND"
      "SOCKS_SERVER"
    ];

    nativeBuildInputs = [ pkgs.bun ];

    dontConfigure = true;

    buildPhase = ''
      runHook preBuild

      export BUN_INSTALL_CACHE_DIR=$(mktemp -d)
      bun install \
        --cpu="${bunCpu}" \
        --os="${bunOs}" \
        --filter './' \
        --filter './packages/cli' \
        --filter './packages/desktop' \
        --filter './packages/app' \
        --frozen-lockfile \
        --ignore-scripts \
        --no-progress

      bun --bun ${src}/nix/scripts/canonicalize-node-modules.ts
      bun --bun ${src}/nix/scripts/normalize-bun-binaries.ts

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out
      find . -type d -name node_modules -exec cp -R --parents {} $out \;

      runHook postInstall
    '';

    dontFixup = true;

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash =
      {
        x86_64-linux = "sha256-XWwlhjvmCdt68mIHTUQsmDO/lBzrYjO34/3tob3d1tA=";
        aarch64-linux = "sha256-/310R94wlUd6rSDE9PJFf1FVXSyS6hAan8zi2NqEj4k=";
        aarch64-darwin = "sha256-0NZH0SXQ7e64wiCy2C9sKlygrr41fqe7JRoBLD8rRIg=";
      }.${hostPlatform.system}
      or (throw "OpenCode 2 is unsupported on ${hostPlatform.system}");

    meta.platforms = [
      "aarch64-linux"
      "x86_64-linux"
      "aarch64-darwin"
    ];
  };
in
(pkgs.callPackage "${src}/nix/opencode.nix" { node_modules = nodeModules; }).overrideAttrs (_: {
  patches = [ ./opencode-google-oauth.patch ];
})
