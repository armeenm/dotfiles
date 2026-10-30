{
  config,
  lib,
  pkgs,
  ...
}: let
  opencode = import ./opencode.nix {inherit lib pkgs;};

  # uv's generator works in the build sandbox. opencode's Bun runtime cannot
  # initialize Intl.Segmenter there on Darwin, so its completion is generated
  # once at shell start and cached.
  uvCompletions = pkgs.runCommandLocal "uv-zsh-completion" {
    nativeBuildInputs = [config.programs.uv.package];
  } ''
    HOME=$TMPDIR XDG_CACHE_HOME=$TMPDIR/cache XDG_DATA_HOME=$TMPDIR/share \
      uv generate-shell-completion zsh > $out
  '';

  specsDir = ./carapace/specs;
  handWritten =
    lib.mapAttrs' (name: _: lib.nameValuePair "carapace/specs/${name}" {source = specsDir + "/${name}";})
    (lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".yaml" name) (builtins.readDir specsDir));

  # Families of commands that share one definition; families.nix renders one
  # spec per command name from that single definition.
  families = import ./carapace/families.nix {inherit lib pkgs;};
  familySpecs =
    lib.mapAttrs' (name: source: lib.nameValuePair "carapace/specs/${name}.yaml" {inherit source;})
    families;

  # Long-tail specs produced by util/generate-completions. Kept as one JSON
  # manifest so a package bump can refresh them with one command.
  generatedManifest = builtins.fromJSON (builtins.readFile ./carapace/generated.json);
  generated =
    lib.mapAttrs' (name: body:
      lib.nameValuePair "carapace/specs/${name}.yaml" {
        source = pkgs.writeText "${name}.yaml" (builtins.toJSON ({name = name;}
          // body));
      })
    generatedManifest;

  # Commands whose interface is the same as an existing completer. One zsh
  # function is shared by every alias and rewrites the command word before
  # calling the canonical completer. Kept in JSON so the coverage check reads
  # the same mapping.
  delegates = builtins.fromJSON (builtins.readFile ./carapace/delegates.json);
  fileOnly = builtins.fromJSON (builtins.readFile ./carapace/file-only.json);

  delegateCode = ''
    typeset -gA _completion_delegates
    _completion_delegate() {
      local canonical=''${_completion_delegates[''$words[1]]}
      [[ -n ''$canonical ]] || return 1

      local -a prefix=(''${(z)canonical})
      local target=''${_comps[''$prefix[1]]}
      [[ -n ''$target ]] || return 1

      local -a saved_words=("''${words[@]}")
      local saved_current=''$CURRENT saved_service=''$service saved_context=''$curcontext

      words=(''$prefix "''${(@)words[2,-1]}")
      (( CURRENT += ''${#prefix} - 1 ))
      service=''$prefix[1]

      ''$target

      words=("''${saved_words[@]}")
      CURRENT=''$saved_current
      service=''$saved_service
      curcontext=''$saved_context
    }

    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (canonical: names: ''
        ${lib.concatMapStringsSep "\n" (name: "_completion_delegates[${name}]=${canonical}") names}
        compdef _completion_delegate ${lib.concatStringsSep " " names}
      '')
      delegates)}
  '';
in {
  # Sourced last so these win over carapace's own `uv` spec.
  programs.zsh.initContent = lib.mkAfter ''
    source ${uvCompletions}
    compdef _uv uv uvx

    _opencode_completion=${config.xdg.cacheHome}/zsh/opencode-completion.zsh
    if [[ ! -s $_opencode_completion || ${opencode}/bin/opencode -nt $_opencode_completion ]]; then
      mkdir -p ${config.xdg.cacheHome}/zsh
      ${opencode}/bin/opencode --completions zsh >| $_opencode_completion 2>/dev/null || rm -f $_opencode_completion
    fi
    [[ -s $_opencode_completion ]] && source $_opencode_completion
    unset _opencode_completion

    ${delegateCode}

    compdef _files ${lib.escapeShellArgs fileOnly}
  '';

  xdg.configFile = handWritten // familySpecs // generated;
}
