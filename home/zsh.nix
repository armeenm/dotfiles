{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) hostPlatform;

  # Commands carapace covers without any bridges. The zsh bridge setup uses
  # this to tell apart "carapace has its own completer" (keep carapace) from
  # "the bash bridge merely picked up a bash completion" (restore native zsh).
  carapaceBaseCompleters = pkgs.runCommandLocal "carapace-base-completers" {
    nativeBuildInputs = [ config.programs.carapace.package ];
  } ''
    carapace _carapace zsh \
      | sed -n 's/^compdef _carapace_completer //p' \
      | grep -o '"[^"]*"' | tr -d '"' | sort -u > $out
  '';
in {
  programs.zsh = {
    enable = true;
    autocd = true;
    autosuggestion.enable = true;
    defaultKeymap = "viins";
    dotDir = "${config.xdg.configHome}/zsh";
    enableCompletion = false; # Handled by carapace.
    enableVteIntegration = true;

    history = {
      path = "${config.xdg.cacheHome}/zsh/history";
      ignoreSpace = true;
    };

    syntaxHighlighting = {
      enable = true;
      highlighters = [ "brackets" ];
      patterns = {
        "rm -rf *" = "fg=white,bold,bg=red";
      };
    };

    profileExtra = ''
      if command -v uwsm &> /dev/null && uwsm check may-start -q && uwsm select; then
        exec uwsm start default
      fi
    '';

    initContent = lib.mkBefore ''
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi
      [[ ! -f ~/.config/zsh/p10k.zsh ]] || source ~/.config/zsh/p10k.zsh

      CARAPACE_BASE_COMPLETERS=${carapaceBaseCompleters}
      . ${./init.zsh}
    '';

    zsh-abbr = {
      enable = true;

      abbreviations = let
        editor = config.home.sessionVariables.EDITOR;
      in {
        b2 = "buck2";
        bz = "bazel";
        ibz = "ibazel";

        cat = "bat";
        ccm = "clipcat-menu";
        g = "git";
        n = "nix";
        nb = "numbat";
        nfu = "nix flake update";
        pls = "sudo !!";
        rlf = "readlink -f";
        rscp = "rsync -ahvP";
        sw = "nh os switch";
        tf = "terraform";
        zj = "zellij";
      } // lib.optionalAttrs hostPlatform.isLinux {
        open = "xdg-open";

        jc = "journalctl";
        jcu = "journalctl --user";
        sc = "systemctl";
        uc = "systemctl --user";
        udc = "udisksctl";
      } // lib.optionalAttrs hostPlatform.isDarwin {
        lc = "launchctl";
      };

      globalAbbreviations = {
        G = "| rg";
        T = "| tv";
        L = "| less -R";
      };
    };
  };
}
