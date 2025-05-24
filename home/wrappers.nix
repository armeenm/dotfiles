{ lib, pkgs, inputs, ... }:

let
  securityWrapper =
  sourceProg:
  pkgs.pkgsStatic.callPackage
    "${inputs.nixpkgs.outPath}/nixos/modules/security/wrappers/wrapper.nix" {
    inherit sourceProg;

    # glibc definitions of insecure environment variables
    #
    # We extract the single header file we need into its own derivation,
    # so that we don't have to pull full glibc sources to build wrappers.
    #
    # They're taken from pkgs.glibc so that we don't have to keep as close
    # an eye on glibc changes. Not every relevant variable is in this header,
    # so we maintain a slightly stricter list in wrapper.c itself as well.
    unsecvars = lib.overrideDerivation (pkgs.srcOnly pkgs.glibc) (
      { name, ... }:
      {
        name = "${name}-unsecvars";
        installPhase = ''
          mkdir $out
          cp sysdeps/generic/unsecvars.h $out
        '';
      }
    );
  };

  mkSetcapProgramCreator =
    {
      name,
      program,
      capabilities,
      source,
      owner,
      group,
      permissions,
      ...
    }: pkgs.writeShellScriptBin name
    ''
      set -euo pipefail
      wrapperDir=$HOME/.local/bin
      src=${securityWrapper source}/bin/security-wrapper
      dst=$wrapperDir/${program}

      if [ -z ''${VERBOSE+x} ]; then
        verboseEcho=:
      else
        verboseEcho=echo
        set -x
      fi

      if cmp -s "$src" "$dst"; then
        verboseEcho Installed wrapper for "${program}" matches; skipping installation.
        exit
      fi

      if [[ $EUID -ne 0 ]]; then
        verboseEcho Rerunning setcap program generator for "${program}" as root...
        exec sudo ${pkgs.runtimeShell} "$0" "$@"
      fi

      cp ${securityWrapper source}/bin/security-wrapper "$wrapperDir/${program}"

      # Prevent races
      chmod 0000 "$wrapperDir/${program}"
      chown ${owner}:${group} "$wrapperDir/${program}"

      # Set desired capabilities on the file plus cap_setpcap so
      # the wrapper program can elevate the capabilities set on
      # its file into the Ambient set.
      ${pkgs.libcap.out}/bin/setcap "cap_setpcap,${capabilities}" "$wrapperDir/${program}"

      # Set the executable bit
      chmod ${permissions} "$wrapperDir/${program}"
    '';

  setup-sunshine = mkSetcapProgramCreator {
    name = "setup-sunshine";
    program = "sunshine";
    capabilities = "cap_sys_admin+p";
    source = "${pkgs.sunshine}/bin/sunshine";
    owner = "root";
    group = "wheel";
    permissions = "110";
  };

in {
  home.activation.setupSunshine = lib.hm.dag.entryAfter ["writeBoundary"] ''
    run setup-sunshine
  '';
}
