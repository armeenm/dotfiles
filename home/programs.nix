{ config
, isHeadless
, isPortable
, enableSocial
, pkgs
, lib
, user
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;
in {
  programs = {
    aria2.enable = true;
    bacon.enable = true;
    bashmount.enable = true;
    bat.enable = true;
    carapace.enable = true;
    clock-rs.enable = true;
    dircolors.enable = true;
    fastfetch.enable = true;
    git-worktree-switcher.enable = true;
    gitui.enable = true;
    home-manager.enable = true;
    htop.enable = true;
    imv.enable = !isHeadless;
    mergiraf.enable = true;
    mpv.enable = !isHeadless;
    navi.enable = true;
    nix-index-database.comma.enable = true;
    nix-init.enable = true;
    nix-your-shell.enable = true;
    noti.enable = !isHeadless;
    pay-respects.enable = true;
    ripgrep-all.enable = true;
    ripgrep.enable = true;
    scmpuff.enable = true;
    yazi.enable = true;
    zoxide.enable = true;

    alacritty = {
      enable = !isHeadless;
      settings = {
        window = {
          dynamic_padding = true;
          option_as_alt = "OnlyLeft";
        };
      };
    };

    atuin = {
      enable = true;
      settings = {
        auto_sync = false;
        enter_accept = true;
        filter_mode_shell_up_key_binding = "session";
        inline_height = 30;
        invert = false;
        show_preview = true;
        style = "compact";
        update_check = false;
        workspaces = true;
      };
    };

    bash = {
      enable = true;
      historyFile = "${config.xdg.cacheHome}/bash/history";
    };

    beets = {
      enable = true;
      package = pkgs.stable.beets;
      settings = {
        directory = config.xdg.userDirs.music;
      };
    };

    btop = {
      enable = true;
      settings = {
        rounded_corners = false;
        vim_keys = true;
        log_level = "WARNING";
      };
    };

    cava = {
      enable = true;
      settings = {
        general.framerate = 144;
      };
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    emacs = {
      enable = true;
      package = pkgs.emacsWithPackagesFromUsePackage {
        config = pkgs.writeTextFile {
          text = ''
            ${builtins.readFile ../conf/emacs/init.el}
            ${config.programs.emacs.extraConfig}
          '';
          name = "config.el";
        };

        override = epkgs: epkgs // {
          emsg-blame = epkgs.trivialBuild {
            pname = "emsg-blame";
            version = "unstable-2025-02-22";

            src = pkgs.fetchFromGitHub {
              owner = "ISouthRain";
              repo = "emsg-blame";
              rev = "7b0bdae8398a38b0bdb103f8cdeaaf62053496cb";
              hash = "sha256-bI3zBUJ/B2TsyQx4N8fmIrACEUEw0IX3OBpNGzVpq2Y=";
            };

            packageRequires = [ epkgs.async ];
          };
        };

        defaultInitFile = true;
        alwaysEnsure = true;
        alwaysTangle = true;
        package = pkgs.emacs-git-pgtk;

        extraEmacsPackages = epkgs: (config.programs.emacs.extraPackages epkgs) ++ (with epkgs; [
          treesit-grammars.with-all-grammars
        ]);
      };
    };

    eza = {
      enable = true;
      git = true;
    };

    firefox = {
      enable = true;

      nativeMessagingHosts = with pkgs; [
        fx-cast-bridge
      ];
    };

    foot = {
      enable = hostPlatform.isLinux && !isHeadless;
      server.enable = true;

      settings = {
        mouse.hide-when-typing = "yes";
        scrollback.lines = 1000000;

        key-bindings.pipe-command-output =
          ''[sh -c "f=$(mktemp); cat - > $f; emacsclient -c $f; rm $f"] Control+Shift+g'';

        main = {
          #font = lib.mkForce "Tamsyn:size=12";
          term = "xterm-256color";
          pad = "20x20";
        };
      };
    };

    gh = {
      enable = true;
      extensions = with pkgs; [
        gh-eco
      ];
    };

    gh-dash.enable = true;

    git = {
      enable = true;
      userEmail = user.email;
      userName = user.name;

      aliases = {
        a = "add";
        aa = "add -A";
        br = "branch";
        ci = "commit";
        co = "checkout";
        d = "diff";
        ds = "diff --staged";
        f = "fuzzy";
        pl = "pull";
        ps = "push";
        psf = "push --force-with-lease";
        r = "rebase";
        st = "status";
        sw = "switch";
        wt = "worktree";
      };

      ignores = [
        ".aider*"
        "!.aider.conf.yml"
        "!.aiderignore"
      ];

      delta = {
        enable = true;
        options = {
          syntax-theme = "base16-stylix";
          line-numbers = true;
        };
      };

      extraConfig = {
        advice.addEmptyPathspec = false;
        core.editor = ''${config.home.sessionVariables.EDITOR}'';
        credential.helper = "store";
        init.defaultBranch = "master";
        push.autoSetupRemote = true;

        diff.guitool = "meld";
        "difftool \"meld\"".cmd = ''meld "$LOCAL" "$REMOTE"'';

        merge.guitool = "meld";
        "mergetool \"meld\"".cmd = ''meld "$LOCAL" "$BASE" "$REMOTE" --output "$MERGED"'';
      };
    };

    hyprlock = {
      enable = hostPlatform.isLinux && !isHeadless;
      settings = {
        auth."fingerprint:enabled" = true;
        general = {
          grace = 3;
          hide_cursor = true;
        };
      };
    };

    less = {
      enable = true;
      keys = ''
        #env

        #command
        / forw-search ^W
      '';
    };

    lesspipe.enable = true;

    mods = {
      enable = enableSocial;
      settings = {
        default-api = "openrouter";
        default-model = "sonnet";

        apis.openrouter = {
          base-url = "https://openrouter.ai/api/v1";
          api-key-env = "OPENROUTER_API_KEY";
          models = {
            "anthropic/claude-3.5-haiku" = {
              aliases = [ "haiku" ];
              max-input-chars = 200000;
            };

            "anthropic/claude-sonnet-4" = {
              aliases = [ "sonnet" ];
              max-input-chars = 200000;
            };

            "anthropic/claude-opus-4" = {
              aliases = [ "opus" ];
              max-input-chars = 200000;
            };

            "deepseek/deepseek-r1-0528" = {
              aliases = [ "r1" ];
              max-input-chars = 128000;
            };

            "google/gemini-2.5-pro-preview" = {
              aliases = [ "gemini-pro" ];
              max-input-chars = 1048576;
            };

            "google/gemini-2.5-flash-preview-05-20" = {
              aliases = [ "gemini-flash" ];
              max-input-chars = 1048576;
            };

            "openai/gpt-4.1" = {
              aliases = [ "gpt" ];
              max-input-chars = 1047576;
            };

            "openai/o3-mini-high" = {
              aliases = [ "o3mini" ];
              max-input-chars = 200000;
            };

            "x-ai/grok-3-beta" = {
              aliases = [ "grok" ];
              max-input-chars = 131072;
            };
          };
        };

        roles.shell = [
          "you are a shell expert"
          "you do not explain anything"
          "you simply output one liners to solve the problems you're asked"
          "you do not provide any explanation whatsoever, ONLY the command"
        ];
      };
    };

    nh = {
      enable = true;
      flake = "${config.home.homeDirectory}/src/dotfiles";
    };

    numbat = {
      enable = true;
      settings = {
        intro-banner = "short";
        prompt = "> ";
      };
    };

    readline = {
      enable = true;
      extraConfig = ''
      set editing-mode vi
    '';
    };

    skim = {
      enable = true;
      defaultOptions = [ "--ansi" ];
    };

    ssh = {
      enable = true;
      compression = true;
      controlMaster = "auto";
    };

    tealdeer = {
      enable = true;
      settings = {
        updates.auto_update = true;
      };
    };

    television = {
      enable = true;
      enableZshIntegration = false;
      settings = {
        keybindings = {
          quit = [ "esc" "ctrl-c" ];
        };
      };
    };

    translate-shell = {
      enable = enableSocial;
      settings = {
        hl = "en";
        tl = [ "hi" ];
      };
    };

    tofi = {
      enable = true;
      settings = {
        "width" = "100%";
        "height" = "100%";
        "border-width" = 0;
        "outline-width" = 0;
        "padding-left" = "35%";
        "padding-top" = "35%";
        "result-spacing" = 25;
        "num-results" = 5;
        "font" = lib.mkForce "${pkgs.fira-code}/share/fonts/truetype/FiraCode-VF.ttf";
        "font-size" = lib.mkForce "";
      };
    };

    vscode = {
      enable = true;
      package = pkgs.vscode.fhs;
      profiles.default = {
        userSettings = {
          "vim.foldfix" = true;
        };
      };
    };

    yt-dlp = {
      enable = enableSocial;
      settings = {
        embed-thumbnail = true;
        downloader = "aria2c";
        downloader-args = "aria2c:'-c -x8 -s8 -k1M'";
      };
    };

    zathura = {
      enable = hostPlatform.isLinux && !isHeadless;
      mappings = {
        "<C-i>" = "recolor";
      };
    };

    zellij = {
      enable = true;
      enableZshIntegration = false;

      settings = {
        pane_frames = false;
        default_layout = "compact";

        keybinds = {
          unbind = "Ctrl g";

          normal = {
            "bind \"Ctrl m\"" = { SwitchToMode = "Locked"; };
          };

          locked."bind \"Ctrl m\"" = { SwitchToMode = "Normal"; };
        };
      };
    };
  };
}
