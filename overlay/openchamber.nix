# OpenChamber desktop (Electron) built from source.
#
# Upstream ships AppImage/deb/rpm built by electron-builder and bundles its own
# copy of the OpenCode CLI. We instead build the workspace with bun, wrap the
# nixpkgs Electron runtime around it, and resolve the OpenCode CLI from the
# environment at runtime (see home/home.nix), so the app uses the same
# `opencode` package as the rest of the config.
{
  lib,
  stdenv,
  stdenvNoCC,
  fetchFromGitHub,
  bun,
  nodejs,
  electron,
  makeWrapper,
  git,
  openssh,
  cloudflared,
}:

let
  pname = "openchamber-desktop";
  version = "2.3.3";

  src = fetchFromGitHub {
    owner = "openchamber";
    repo = "openchamber";
    rev = "v${version}";
    hash = "sha256-WLXYk8Yq/UI4uLCbdzGZrR96lNf3pIEHBPny1ELREfw=";
  };

  # `bun install` needs the network, so the dependency trees are fixed-output
  # derivations. Every link bun writes is relative (including the workspace
  # links under packages/*/node_modules), so copying a tree out verbatim keeps
  # it self-consistent; the matching directory layout is restored in the build
  # and install phases below.
  #
  # Two trees are needed: the full one to build (vite, typescript, ...) and a
  # production-only one to ship, mirroring how electron-builder packages the
  # app. Shipping the full tree would drag in the packaging toolchain
  # (~600M, mostly app-builder-bin).
  mkNodeModules =
    { name, flags, hash }:
    stdenvNoCC.mkDerivation {
      pname = "${pname}-${name}";
      inherit version src;

      nativeBuildInputs = [ bun ];

      buildPhase = ''
        runHook preBuild
        export HOME=$TMPDIR
        bun install --frozen-lockfile --ignore-scripts ${flags}
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        mkdir -p $out
        cp -r node_modules $out/
        for dir in packages/*; do
          if [ -d "$dir/node_modules" ]; then
            mkdir -p "$out/$dir"
            cp -r "$dir/node_modules" "$out/$dir/"
          fi
        done
        runHook postInstall
      '';

      outputHashAlgo = "sha256";
      outputHashMode = "recursive";
      outputHash = hash;

      # bun points some packages/*/node_modules/.bin entries at workspace
      # packages (@openchamber/web, @openchamber/sdk) that are not part of the
      # dependency tree, so they dangle here and resolve only once the build
      # copies the sources in next to them.
      dontFixup = true;
    };

  nodeModulesBuild = mkNodeModules {
    name = "node-modules";
    flags = "";
    hash = "sha256-Dw2qUNnUP5ZVZo4ASPHXAoKSmIIrp7xWjqblQKjPch4=";
  };

  nodeModulesRuntime = mkNodeModules {
    name = "node-modules-runtime";
    flags = "--production";
    hash = "sha256-ATzIZmhN9YKpbk73J6QNRiad6+bT9s6SXU7gKtxMavs=";
  };

  # Lays the dependency tree out next to the sources at the depth bun's
  # relative links expect.
  copyNodeModules = from: dest: ''
    cp -r ${from}/node_modules ${dest}/node_modules
    chmod -R u+w ${dest}/node_modules
    for dir in ${from}/packages/*; do
      pkg=$(basename "$dir")
      if [ -d "$dir/node_modules" ]; then
        mkdir -p "${dest}/packages/$pkg"
        cp -r "$dir/node_modules" "${dest}/packages/$pkg/node_modules"
        chmod -R u+w "${dest}/packages/$pkg/node_modules"
      fi
    done
  '';
in
stdenv.mkDerivation {
  inherit pname version src;

  nativeBuildInputs = [
    bun
    nodejs
    makeWrapper
  ];

  buildPhase = ''
    runHook preBuild
    export HOME=$TMPDIR
    # build-web-assets.mjs probes `bash -lc 'command -v bun'` when BUN is unset,
    # and /bin/bash does not exist in the sandbox.
    export BUN=${bun}/bin/bun

    chmod -R u+w .
    ${copyNodeModules nodeModulesBuild "."}
    chmod -R u+w node_modules packages
    patchShebangs node_modules

    # The web server imports @openchamber/sdk from its built dist/; the root
    # postinstall would normally do this, but it runs with --ignore-scripts.
    bun run --cwd packages/sdk build
    # Builds packages/web into packages/electron/resources/web-dist. This also
    # runs scripts/build-builtin-extensions.mjs (as part of packages/web's own
    # build script).
    bun run --cwd packages/electron build:web-assets
    # Bundles entry/main/early-startup into packages/electron/dist-bundle.
    bun run --cwd packages/electron bundle:main

    # Not running under electron-builder, so the runtime resolves its
    # resources relative to the bundle directory instead of resourcesPath.
    ln -s ../resources packages/electron/dist-bundle/resources
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/libexec/openchamber-desktop
    cp package.json $out/libexec/openchamber-desktop/
    cp -r packages $out/libexec/openchamber-desktop/

    # Swap the build-time dependency tree for the production-only one.
    rm -rf $out/libexec/openchamber-desktop/packages/*/node_modules
    ${copyNodeModules nodeModulesRuntime "$out/libexec/openchamber-desktop"}

    # The bundled main process keeps @openchamber/web external, so Node has to
    # resolve it from the application root at runtime.
    mkdir -p $out/libexec/openchamber-desktop/node_modules/@openchamber
    ln -s ../../packages/web $out/libexec/openchamber-desktop/node_modules/@openchamber/web

    mkdir -p $out/share/icons/hicolor/scalable/apps
    cp packages/electron/resources/icons/app-icon.svg \
      $out/share/icons/hicolor/scalable/apps/openchamber.svg

    mkdir -p $out/bin
    makeWrapper ${lib.getExe electron} $out/bin/openchamber-desktop \
      --add-flags "$out/libexec/openchamber-desktop/packages/electron" \
      --prefix PATH : ${lib.makeBinPath [ git openssh cloudflared ]} \
      --set NODE_ENV production

    runHook postInstall
  '';

  meta = {
    description = "Desktop GUI for the OpenCode AI coding agent";
    homepage = "https://github.com/openchamber/openchamber";
    license = lib.licenses.mit;
    mainProgram = "openchamber-desktop";
    platforms = lib.platforms.linux;
  };
}
