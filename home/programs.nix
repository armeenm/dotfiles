args@{ config, pkgs, lib, inputs, root, user, ... }:

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
        ayu = {
          src = pkgs.fetchFromGitHub {
            owner = "dempfi";
            repo = "ayu";
            rev = "4.0.3";
            hash = "sha256-O0zoKAmCgSAHv2gcORYrorIlw0kdXN1+2k2Emtntc2g=";
          };
          file = "ayu-dark.tmTheme";
        };
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

    hyprlock = {
      enable = true;
      settings = {
        general = {
          disable_loading_bar = true;
          grace = 3;
          hide_cursor = true;
          no_fade_in = false;
        };

        background = [
          {
            path = "screenshot";
            blur_passes = 5;
            blur_size = 8;
          }
        ];

        input-field = [
          {
            size = "200, 50";
            position = "0, -80";
            monitor = "";
            dots_center = true;
            fade_on_empty = false;
            font_color = "rgb(202, 211, 245)";
            inner_color = "rgb(91, 96, 120)";
            outer_color = "rgb(24, 25, 38)";
            outline_thickness = 5;
            placeholder_text = ''<span foreground="##cad3f5">Password...</span>'';
            shadow_passes = 2;
          }
        ];
      };
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

      initExtraFirst = ''
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi
      [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

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
