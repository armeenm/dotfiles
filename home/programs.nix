{ config
, osConfig
, isHeadless
, pkgs
, lib
, inputs
, root
, user
, ...
} @ args:

let
  inherit (osConfig.nixpkgs) hostPlatform;
in {
  programs = {
    aria2.enable = true;
    bat.enable = true;
    dircolors.enable = true;
    fzf.enable = true;
    home-manager.enable = true;
    nix-index.enable = true;
    noti.enable = true;
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
            ${builtins.readFile "${root}/conf/emacs/init.el"}
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

    foot = {
      enable = hostPlatform.isLinux && !isHeadless;
      server.enable = true;

      settings = {
        mouse.hide-when-typing = "yes";
        key-bindings.pipe-command-output = ''[sh -c "f=$(mktemp); cat - > $f; emacsclient -c $f; rm $f"] Control+Shift+g'';

        main = {
          term = "xterm-256color";
          pad = "20x20";
        };
      };
    };

    gh = {
      enable = true;
      extensions = with pkgs; [
        gh-dash
        gh-eco
      ];
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

    hyprlock = {
      enable = hostPlatform.isLinux && !isHeadless;
      settings = {
        general = {
          disable_loading_bar = true;
          grace = 3;
          hide_cursor = true;
          no_fade_in = false;
        };
      };
    };

    mpv = {
      enable = !isHeadless;
      config = {
        gpu-api = "vulkan";
        gpu-context = "wayland";
        hwdec = "vaapi";
        profile = "gpu-hq";
        spirv-compiler = "auto";
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
      enable = false;
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

    ssh = {
      enable = true;
      compression = true;
      controlMaster = "auto";
    };

    starship = import ./starship.nix args;

    tealdeer = {
      enable = true;
      settings = {
        updates.auto_update = true;
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

    waybar = {
      enable = hostPlatform.isLinux && !isHeadless;
      systemd.enable = true;

      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 40;

          output = [ "*" ];

          modules-left = [
            "hyprland/workspaces"
            "hyprland/submap"
            "custom/separator0"
            "group/mpris"
          ];

          modules-center = [ "hyprland/window" ];

          modules-right = [
            "cava"
            "custom/separator1"
            "group/pulse"
            "custom/separator0"
            "bluetooth"
            "custom/separator0"
            "network"
            "custom/separator0"
            "cpu"
            "temperature"
            "memory"
            "custom/separator0"
            "clock"
            "custom/separator2"
            #"backlight"
            #"battery"
          ];

          "group/pulse" = {
            orientation = "inherit";
            drawer = {};
            modules = [
              "pulseaudio"
              "pulseaudio/slider"
            ];
          };

          "group/mpris" = {
            orientation = "inherit";
            drawer = {
              click-to-reveal = true;
              transition-left-to-right = false;
            };
            modules = [
              "custom/whitespace"
              "mpris"
            ];
          };

          "custom/separator0".format = " | ";
          "custom/separator1".format = " ";
          "custom/separator2".format = "  ";
          "custom/whitespace".format = lib.strings.replicate 10 " ";

          bluetooth = {
            on-click = "footclient bluetuith";
          };

          cava = {
            actions.on-click-right = "mode";
            autosens = 1;
            bar_delimiter = 0;
            bars = 14;
            format-icons = ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"];
            framerate = 60;
            sleep_timer = 5;
            hide_on_silence = true;
            waves = false;
            lower_cutoff_freq = 1;
            higher_cutoff_freq = 10000;
          };

          clock = {
            format = "{:%H:%M:%S %Z}";
            format-alt = "{:%A, %B %d, %Y (%R %Z)}";
            tooltip-format = "{tz_list}";
            interval = 1;
            timezones = [
              "America/Los_Angeles"
              "America/New_York"
              "Asia/Kolkata"
              "Etc/UTC"
            ];
          };

          cpu = {
            format = "CPU: {}%";
          };

          memory = {
            format = "RAM: {}%";
          };

          pulseaudio = {
            format = "{volume}% {icon}";
            format-bluetooth = "{volume}% {icon}";
            format-muted = "MUTE ";
            format-icons = {
              default = ["" ""];
            };
            on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
            on-click-middle = "${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            scroll-step = 2.0;
            ignored-sinks = [ "Easy Effects Sink" ];
          };

          mpris = {
            format = "{player_icon} {status_icon} {dynamic}";
            interval = 1;
            dynamic-len = 100;
            player-icons = {
              default = "";
              firefox = "";
              Feishin = "";
            };
            status-icons = {
              playing = "";
              paused = "";
            };
          };

          "hyprland/window" = {
            separate-outputs = true;
            rewrite."(\\[Sidebery\\] )?(.*) — Mozilla Firefox" = " $2";
          };
        };
      };

      style = ''
        window#waybar {
          background: transparent;
        }
        #pulseaudio-slider slider {
          min-height: 0px;
          min-width: 0px;
          opacity: 0;
          background-image: none;
          border: none;
          box-shadow: none;
        }
        #pulseaudio-slider trough {
          min-height: 2px;
          min-width: 80px;
          border-radius: 0px;
          background-color: black;
        }
        #pulseaudio-slider highlight {
          min-width: 2px;
          border-radius: 0px;
          background-color: white;
        }
        * {
          font-family: "Cozette";
          font-size: 11pt;
        }
      '';
    };

    yt-dlp = {
      enable = !isHeadless;
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

    zsh = {
      enable = true;
      autosuggestion.enable = true;
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

      profileExtra = ''
        if uwsm check may-start -q && uwsm select; then
          exec uwsm start default
        fi
      '';

      initContent = lib.mkBefore ''
        source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
        if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
          source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
        fi
        [[ ! -f ~/.config/zsh/p10k.zsh ]] || source ~/.config/zsh/p10k.zsh

        function zshaddhistory() { return 1 }

        bindkey '^ ' autosuggest-accept
        _zsh_autosuggest_strategy_atuin_top() {
            suggestion=$(atuin search --cmd-only --limit 1 --search-mode prefix $1)
        }

        ZSH_AUTOSUGGEST_STRATEGY=atuin_top

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

        function y() {
          local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
          yazi "$@" --cwd-file="$tmp"
          if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
            builtin cd -- "$cwd"
          fi
          rm -f -- "$tmp"
        }

        zle -N _sgpt_zsh
        bindkey ^p _sgpt_zsh

        function precmd {
          print -Pn "\e]133;A\e\\"
          if ! builtin zle; then
            print -n "\e]133;D\e\\"
          fi
        }

        function preexec {
          print -n "\e]133;C\e\\"
          print -Pn "\e]0;''${(q)1}\e\\"
        }

        function scroll-top() {
          local esc
          local -i ROW COL OFFSET
          IFS='[;' read -sdR $'esc?\e[6n' ROW COL <$TTY
          OFFSET="''${#''${(@Af)PREBUFFER%$'\n'}}"+"''${#''${(@Af)LBUFFER:-1}}"
          (( ROW-OFFSET )) && printf '\e[%1$dS\e[%1$dA' ROW-OFFSET
          zle redisplay
        }
        zle -N clear-screen scroll-top
      '';
    };
  };
}
