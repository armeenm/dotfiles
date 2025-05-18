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
        "font" = "${pkgs.fira-code}share/fonts/truetype/FiraCode-VF.ttf";
        "background-color" = lib.mkForce "#000A";
      };
    };

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
        main = {
          term = "xterm-256color";
          dpi-aware = "no";
          pad = "20x20";
        };

        mouse = {
          hide-when-typing = "yes";
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
      matchBlocks = {
        "i-* mi-*" = {
          proxyCommand = ''
            sh -c "aws ssm start-session --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"
          '';
        };
      };
    };

    starship = import ./starship.nix args;

    tealdeer = {
      enable = true;
      settings = {
        updates.auto_update = true;
      };
    };

    waybar = {
      enable = hostPlatform.isLinux && !isHeadless;
      systemd.enable = true;

      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 24;

          output = [ "DP-1" "DP-2" ];

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
      '';
    };
  };
}
