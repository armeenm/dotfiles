{ inputs, lib, ... }:

final: prev: {
  stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};

  # TODO: Remove when https://github.com/NixOS/nixpkgs/issues/575192 is fixed.
  # `aggregateModules` merges the single-module `.ko.cer` certificates of every
  # external kernel module, but modules that install themselves without going
  # through Kbuild's `modules_install` (e.g. ddcci-driver) never generate one,
  # so the `cat` fails. Skip certificates that are absent.
  aggregateModules = modules:
    (prev.aggregateModules modules).overrideAttrs (old: {
      postBuild = builtins.replaceStrings
        [ "cat >> modules.cer" ]
        [ "sh -c 'for f do if [ -e \"$f\" ]; then cat \"$f\"; fi; done' sh >> modules.cer" ]
        old.postBuild;
    });

  # libgit2's test suite fails in the macOS build sandbox
  # (checkout::icase::overwrites_links_for_folders_when_forced returns EINVAL).
  # Determinate Nix pulls it in; the tests are skipped on Darwin only.
  libgit2 = prev.libgit2.overrideAttrs (_: {
    doCheck = !prev.stdenv.hostPlatform.isDarwin;
  });

  # TODO: Remove when clipcat allows `renamed_and_removed_lints`. Its workspace
  # denies that lint, and rustc 1.99 removed several lints the workspace also
  # denies (e.g. `no_mangle_generic_items`), which turns the build into an error.
  clipcat = prev.clipcat.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + ''
      sed -i 's/^renamed_and_removed_lints.*"deny"/renamed_and_removed_lints = "allow"/' Cargo.toml
    '';
  });

  dufs = prev.dufs.overrideAttrs (_: {
    doCheck = false;
  });

  # TODO: Remove when https://github.com/shezdy/hyprsplit/issues/87 is fixed upstream.
  hyprsplit = inputs.hyprsplit.packages.${final.stdenv.hostPlatform.system}.hyprsplit.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./hyprsplit.patch ];
  });

  # TODO: Remove when glean-parser's date tests stop reading SOURCE_DATE_EPOCH.
  # glean-parser 20.2.0 treats a metric as expired relative to SOURCE_DATE_EPOCH
  # when it is set, and nixpkgs exports it as 1980-01-01, so its date-based
  # tests fail. Unset it for the test run (mozillavpn is its only consumer).
  mozillavpn = (prev.mozillavpn.override {
    python3 = prev.python3.override {
      packageOverrides = _self: super: {
        glean-parser = super.glean-parser.overridePythonAttrs (old: {
          preCheck = (old.preCheck or "") + "\nunset SOURCE_DATE_EPOCH\n";
        });
      };
    };
  }).overrideAttrs (_: {
    patches = [ ./mozillavpn.patch ];
  });

  # TODO: Remove when gtksourceview's test-search-context stops aborting under
  # Xvfb in the GitHub Actions sandbox ("cannot open display: :99"), where the
  # other 22 tests in the same run pass. The cached build is unaffected.
  gtksourceview4 = prev.gtksourceview4.overrideAttrs (_: {
    doCheck = false;
  });

  fwupd = prev.fwupd.overrideAttrs (old: {
    mesonFlags = old.mesonFlags ++ [
      (lib.mesonOption "efi_app_location" "/run/fwupd-efi")
    ];
  });

  # OpenChamber 2.5.0 uses `webContents.setZoomMode`, which Electron 44 added.
  # nixpkgs' `electron` is still 43, so pin the desktop app to Electron 44.
  openchamber-desktop = final.callPackage ./openchamber.nix {
    electron = final.electron_44;
  };

  # CVE-2026-111497 (CVSS 9.2): a crafted brotli stream could overflow the
  # decoder's buffer in 1.2.0. 1.2.1 fixes it. python3Packages.brotli inherits
  # version and src from this package, so it follows automatically.
  # The LoongArch64/IA64 build fix is not in 1.2.1 yet, so the existing patch
  # is kept.
  brotli = prev.brotli.overrideAttrs (old: {
    version = "1.2.1";
    src = prev.fetchFromGitHub {
      owner = "google";
      repo = "brotli";
      tag = "v1.2.1";
      hash = "sha256-llY72cYffDv0ugsH0p89xIPEbElV4L8u7U/Fe8hT3gI=";
    };

    # 1.2.1 already contains the CVE fix. nixpkgs' nodejs (22.x and 26.x)
    # still appends that same commit to `patches`; patch(1) sees it as reversed
    # and aborts. Skip the automatic patch phase and apply only the patches
    # that were on nixpkgs' 1.2.0 build (currently LoongArch64/IA64).
    #
    # The patches must be applied in postUnpack, not postPatch: `dontPatch`
    # skips the whole patch phase, and the postPatch hook lives inside it.
    # postUnpack runs before stdenv cds into $sourceRoot, so cd explicitly.
    dontPatch = true;
    postUnpack = (old.postUnpack or "") + lib.concatMapStrings (p: ''
      (cd "$sourceRoot" && patch -p1 < ${p})
    '') (old.patches or [ ]);
  });

  # CVE-2026-111497 for the CFFI binding. It links against the system brotli
  # above (USE_SHARED_BROTLI=1), but bump the binding as well so version
  # scanners see the patched release.
  python3Packages = prev.python3Packages.override {
    overrides = _self: super: {
      brotlicffi = super.brotlicffi.overridePythonAttrs (_: {
        version = "1.2.1.0";
        src = prev.fetchFromGitHub {
          owner = "python-hyper";
          repo = "brotlicffi";
          tag = "v1.2.1.0";
          hash = "sha256-N2zxPLsCaXcLpthGvbaKerWayo3ZGMOQ0Is5cpgXGKA=";
        };
      });
    };
  };

  # CVE-2026-111497 in bun's bundled brotli decoder. Sources are prebuilt
  # per platform; bump the URL and hash together. OpenChamber declares
  # `packageManager: bun@1.4.4`, so this also matches its expectation.
  bun = prev.bun.overrideAttrs (old: {
    version = "1.4.4";
    src = prev.fetchurl {
      url = "https://github.com/oven-sh/bun/releases/download/bun-v1.4.4/bun-${
        {
          aarch64-darwin = "darwin-aarch64";
          aarch64-linux = "linux-aarch64";
          x86_64-linux = "linux-x64-baseline";
        }.${prev.stdenvNoCC.hostPlatform.system}
          or (throw "bun: unsupported system ${prev.stdenvNoCC.hostPlatform.system}")
      }.zip";
      hash = {
        aarch64-darwin = "sha256-wkDX8My5DCY6hJ/NsJt+5iYw2SE0jtaq/ahrnVd7xRA=";
        aarch64-linux = "sha256-uqa7NvUdblhWHgK76oJQOKnfRU9nPW13bou/hvjHhQ4=";
        x86_64-linux = "sha256-4ERutVmVdy65iUvrrppr42jZhai5Jx7hJEhvmUbu3XY=";
      }.${prev.stdenvNoCC.hostPlatform.system};
    };
  });

  # Node 24.21.0 bundles brotli 1.2.0 (CVE-2026-111497). The stable channel's
  # nodejs_24 is 24.22.1, which bundles brotli 1.2.1. Use it for both the
  # versioned attribute and the default alias so anything that pulls `nodejs`
  # (e.g. openchamber-desktop) gets the patched decoder.
  nodejs_24 = final.stable.nodejs_24;
  nodejs = final.stable.nodejs;
}
