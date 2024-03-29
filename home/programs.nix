{ config, pkgs, lib, inputs, root, user, ... }:

{
  programs = {
    aria2.enable = true;
    dircolors.enable = true;
    fzf.enable = true;
    home-manager.enable = true;
    nix-index.enable = true;
    noti.enable = true;
    zoxide.enable = true;

    atuin = {
      enable = true;

      settings = {
        auto_sync = false;
        style = "compact";
        enter_accept = true;
        inline_height = 30;
        invert = false;
        show_preview = true;
        update_check = false;
        workspaces = true;
        filter_mode_shell_up_key_binding = "session";
      };
    };

    bash = {
      enable = true;
      historyFile = "${config.xdg.cacheHome}/bash/history";
    };

    bat = {
      enable = true;
      config = {
        theme = "ayu";
      };
      themes = {
        ayu = builtins.readFile (pkgs.fetchFromGitHub
          {
            owner = "dempfi";
            repo = "ayu";
            rev = "4.0.3";
            hash = "sha256-O0zoKAmCgSAHv2gcORYrorIlw0kdXN1+2k2Emtntc2g=";
          } + "/ayu-dark.tmTheme");
      };
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    emacs = {
      enable = true;
      package = pkgs.emacsWithPackagesFromUsePackage {
        config = "${root}/conf/emacs/init.el";
        defaultInitFile = true;
        alwaysEnsure = true;
        package = pkgs.emacs-pgtk;

        override = epkgs: epkgs // {
          xah-wolfram-mode = epkgs.trivialBuild {
            pname = "xah-wolfram-mode";
            version = "";
            src = pkgs.fetchFromGitHub {
              owner = "xahlee";
              repo = "xah-wolfram-mode";
              rev = "d8dbf460ed1b3efd4a1adf45cdf80214b55a39c4";
              hash = "sha256-HvEzqLi7iRx1six8K4mq5M0OlesqXYx9RuGwqfMyqDk=";
            };
          };

          projectile = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.emacsPackages.projectile;
        };

        extraEmacsPackages = epkgs: with epkgs; [
          treesit-grammars.with-all-grammars
        ];
      };
    };

    eza = {
      enable = true;
      enableAliases = true;
      git = true;
    };

    foot = {
      enable = true;
      server.enable = true;

      settings = {
        main = {
          term = "xterm-256color";
          font = "Tamsyn:size=12";
          dpi-aware = "no";
          pad = "20x20";
        };

        mouse = {
          hide-when-typing = "yes";
        };

        colors = {
          # TODO: Base16?
          # Ayu dark theme.
          background = "000919";
          foreground = "c3c0bb";

          regular0 = "242936"; # black
          regular1 = "f28779"; # red
          regular2 = "d5ff80"; # green
          regular3 = "ffd173"; # yellow
          regular4 = "73d0ff"; # blue
          regular5 = "dfbfff"; # magenta
          regular6 = "5ccfe6"; # cyan
          regular7 = "cccac2"; # white

          bright0 = "fcfcfc"; # bright black
          bright1 = "f07171"; # bright red
          bright2 = "86b300"; # bright gree
          bright3 = "f2ae49"; # bright yellow
          bright4 = "399ee6"; # bright blue
          bright5 = "a37acc"; # bright magenta
          bright6 = "55b4d4"; # bright cyan
          bright7 = "5c6166"; # bright white
        };
      };
    };

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
        st = "status";
        sw = "switch";
        wt = "worktree";
      };

      delta = {
        enable = true;
        options = {
          syntax-theme = "ayu";
          line-numbers = true;
        };
      };

      extraConfig = {
        init.defaultBranch = "master";
        credential.helper = "store";
        core.editor = ''${config.home.sessionVariables.EDITOR}'';
        push.autoSetupRemote = true;
      };
    };

    mpv = {
      enable = true;
      config = {
        gpu-api = "vulkan";
        gpu-context = "wayland";
        hwdec = "vaapi";
        profile = "gpu-hq";
        spirv-compiler = "shaderc";
      };
    };

    ncmpcpp = {
      enable = true;
      bindings = [
        { key = "j"; command = "scroll_down"; }
        { key = "k"; command = "scroll_up"; }
        { key = "J"; command = [ "select_item" "scroll_down" ]; }
        { key = "K"; command = [ "select_item" "scroll_up" ]; }
      ];
    };

    nushell = {
      enable = true;

      shellAliases = config.home.shellAliases;

      envFile.text = ''
      $env.PROMPT_INDICATOR_VI_INSERT = ""
      $env.PROMPT_INDICATOR_VI_NORMAL = ""

      $env.config = {
        show_banner: false,
        keybindings: [],
        edit_mode: vi,
        cursor_shape: {
          emacs: line,
          vi_insert: line,
          vi_normal: underscore,
        }
                                        }
    '';
    };

    readline = {
      enable = true;
      extraConfig = ''
      set editing-mode vi
    '';
    };

    starship = {
      enable = true;

      settings = {
        add_newline = false;
        continuation_prompt = ">> ";

        format = lib.concatStrings [
          "$username"
          "$hostname"
          "$localip"
          "$directory"
          "$git_branch"
          "$git_commit"
          "$git_state"
          "$git_status"
          "$fill"
          "$git_metrics"
          "$fill "
          "$time"
          "$shlvl"
          "$status"
          "$shell"
          "$all"
          "$character"
        ];

        aws = {
          format = "[$symbol( $profile)($region)($duration)]($style) ";
          symbol = "aws";
        };

        c = {
          format = "[$symbol( $version(-$name))]($style) ";
          symbol = "c";
        };

        character = {
          success_symbol = "[>](green)";
          error_symbol = "[>](red)";
          vimcmd_symbol = "[<](green)";
        };

        cmake = {
          format = "[$symbol( $version)]($style) ";
          symbol = "cmake";
        };

        cmd_duration = {
          format = "[$duration]($style) ";
        };

        conda = {
          format = "[$symbol $environment]($style) ";
          symbol = "conda";
        };

        dart = {
          format = "[$symbol( $version)]($style) ";
          symbol = "dart";
        };

        docker_context = {
          format = "[$symbol $context]($style) ";
          symbol = "docker";
        };

        fill = {
          symbol = "-";
          style = "dimmed white";
        };

        gcloud = {
          format = "[$symbol $account(@$domain)($region)]($style) ";
          symbol = "gcp";
        };

        git_branch = {
          format = "[$branch]($style) ";
        };

        git_commit = {
          tag_symbol = " tag ";
        };

        git_status = {
          format = "([$all_status$ahead_behind]($style))";
          style = "yellow";

          diverged = "<$behind_count>$ahead_count";

          conflicted = "=$count ";
          ahead = "[>$count](green) ";
          behind = "[<$count](green) ";
          untracked = "?$count ";
          stashed = "*$count ";
          modified = "!$count ";
          staged = "+$count ";
          renamed = "r$count ";
          deleted = "x$count ";
        };

        git_metrics = {
          disabled = false;
          format = "( [+$added]($added_style) )([-$deleted]($deleted_style) )";
        };

        golang = {
          format = "[$symbol( $version)]($style) ";
          symbol = "go";
        };

        gradle = {
          format = "[$symbol( $version)]($style) ";
          symbol = "gradle";
        };

        haskell = {
          format = "[$symbol( $version)]($style) ";
          symbol = "hs";
        };

        helm = {
          format = "[$symbol( $version)]($style) ";
          symbol = "helm";
        };

        java = {
          format = "[$symbol( $version)]($style) ";
          symbol = "java";
        };

        julia = {
          format = "[$symbol( $version)]($style) ";
          symbol = "jl";
        };

        kotlin = {
          format = "[$symbol( $version)]($style) ";
          symbol = "kt";
        };

        kubernetes = {
          format = "[$symbol $context( $namespace)]($style) ";
          symbol = "k8s";
        };

        lua = {
          format = "[$symbol( $version)]($style) ";
          symbol = "lua";
        };

        memory_usage = {
          format = "$symbol [$ram( | $swap)]($style) ";
          symbol = "ram";
        };

        meson = {
          format = "[$symbol $project]($style) ";
          symbol = "meson";
        };

        nix_shell = {
          format = "[$symbol $state( $name)]($style) ";
          symbol = "nix";
        };

        nodejs = {
          format = "[$symbol( $version)]($style) ";
          symbol = "node";
        };

        os = {
          format = "[$symbol]($style) ";
        };

        package = {
          format = "[$symbol $version]($style) ";
          symbol = "pkg";
        };

        purescript = {
          format = "[$symbol( $version)]($style) ";
          symbol = "purs";
        };

        python = {
          format = "[$symbol$pyenv_prefix( $version)($virtualenv)]($style) ";
          symbol = "py";
        };

        ruby = {
          format = "[$symbol( $version)]($style) ";
          symbol = "rb";
        };

        rust = {
          format = "[$symbol( $version)]($style) ";
          symbol = "rs";
        };

        shell = {
          disabled = false;
        };

        shlvl = {
          disabled = false;
          symbol = "lvl ";
        };

        status = {
          disabled = false;
          format = "[$common_meaning]($style) ";
          # TODO: Fix the weird width stuff
          pipestatus = false;
          pipestatus_format = "$pipestatus=> [$common_meaning$signal_name$maybe_int]($style) ";
        };

        sudo = {
          format = "[as $symbol]($style) ";
          symbol = "sudo";
        };

        swift = {
          format = "[$symbol( $version)]($style) ";
          symbol = "swift";
        };

        terraform = {
          format = "[$symbol $workspace]($style) ";
          symbol = "tf";
        };

        time = {
          disabled = false;
          format = "[$time]($style) ";
        };

        username = {
          format = "[$user]($style) ";
        };

        zig = {
          format = "[$symbol( $version)]($style) ";
          symbol = "zig";
        };
      };
    };

    tealdeer = {
      enable = true;
      settings = {
        updates.auto_update = true;
      };
    };

    waybar = {
      enable = true;
      systemd.enable = true;

      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 24;

          modules-left = [
            "hyprland/workspaces"
          ];

          modules-center = [ "hyprland/window" ];

          modules-right = [
            "pulseaudio"
            "network"
            "temperature"
            "cpu"
            "memory"
            "battery"
            "tray"
            "clock"
          ];

          "wlr/workspaces" = {
            disable-scroll = true;
            all-outputs = false;
            on-click = "activate";
          };

          "wlr/mode" = { format = "<span style=\"italic\">{}</span>"; };
          "tray" = {
            # "icon-size" = 21,
            "spacing" = 10;
          };

          "clock" = { "format-alt" = "{:%Y-%m-%d}"; "on-click" = ""; };
          "cpu" = {
            "format" = "{usage}% 󰍛";
          };

          "memory" = { "format"= "{}% "; };

          "temperature" = {
            "critical-threshold" = 80;
            "format" = "{}℃  󰏈";
            "format-critical" = "{}℃ 󰇺";
            "interval" = 5;
          };

          "battery" = {
            "bat"= "BAT0";
            "states"= {
              # "good"= 95;
              "warning"= 30;
              "critical"= 15;
            };
            "format"= "{capacity}% {icon}";
            # "format-good"= ""; # An empty format will hide the module
            # "format-full"= "";
            "format-icons"= ["" "" "" "" ""];
          };

          "network" = {
            "format-wifi"= "{essid} ({signalStrength}%) ";
            "format-ethernet"= "{ifname}= {ipaddr}/{cidr} ";
            "format-disconnected"= "Disconnected ⚠";
          };

          "pulseaudio" = {
            #"scroll-step"= 1;
            "format"= "{volume}% {icon}";
            "format-bluetooth"= "{volume}% {icon}";
            "format-muted"= "";
            "format-icons"= {
              "headphones" = "";
              "handsfree" = "";
              "headset" = "";
              "phone" = "";
              "portable" = "";
              "car" = "";
              "default" = [ "" "" ];
            };
            "on-click"= "pavucontrol";
          };

          "hyprland/window" = {
            "format" = {};
            "seperate-outputs" = true;
          };
        };
      };
    };

    yazi = {
      enable = true;
    };

    yt-dlp = {
      enable = true;
      settings = {
        embed-thumbnail = true;
        downloader = "aria2c";
        downloader-args = "aria2c:'-c -x8 -s8 -k1M'";
      };
    };

    zathura = {
      enable = true;
      mappings = {
        "<C-i>" = "recolor";
      };
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableVteIntegration = true;
      syntaxHighlighting.enable = true;

      autocd = true;
      defaultKeymap = "viins";

      dotDir = "${builtins.baseNameOf config.xdg.configHome}/zsh";

      history = {
        path = "${config.xdg.cacheHome}/zsh/history";
        ignoreSpace = true;
      };

      initExtraFirst = ''
      function zshaddhistory() { return 1 }

      bindkey '^ ' autosuggest-accept
      _zsh_autosuggest_strategy_atuin_top() {
          suggestion=$(atuin search --cmd-only --limit 1 --search-mode prefix $1)
      }

      ZSH_AUTOSUGGEST_STRATEGY=atuin_top
    '';

      initExtraBeforeCompInit = ''
      autoload -Uz zcalc
      autoload -Uz edit-command-line

      zle-keymap-select () {
        if [ $KEYMAP = vicmd ]; then
          printf "\033[2 q"
        else
          printf "\033[6 q"
        fi
      }

      zle -N zle-keymap-select

      zle-line-init () {
        zle -K viins
        printf "\033[6 q"
      }

      zle -N zle-line-init

      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line
      bindkey -v '^?' backward-delete-char

      setopt globdots
      setopt autopushd

      d () {
        diff -u $@ | delta
      }

      _sgpt_zsh() {
      if [[ -n "$BUFFER" ]]; then
          _sgpt_prev_cmd=$BUFFER
          BUFFER+=" processing..."
          zle -I && zle redisplay
          BUFFER=$(sgpt --shell <<< "$_sgpt_prev_cmd")
          zle end-of-line
      fi
      }
      zle -N _sgpt_zsh
      bindkey ^p _sgpt_zsh
    '';
    };
  };
}
