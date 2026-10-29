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
}
