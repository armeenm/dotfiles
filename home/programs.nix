{ config, pkgs, lib, root, user, ... }:

{
  aria2.enable = true;
  dircolors.enable = true;
  fzf.enable = true;
  home-manager.enable = true;
  nix-index.enable = true;
  noti.enable = true;
  zathura.enable = true;
  zoxide.enable = true;

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
      };

      mouse = {
        hide-when-typing = "yes";
      };

      colors = {
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
      init = {
        defaultBranch = "master";
      };

      credential = {
        helper = "store";
      };

      core = {
        editor = "${config.home.sessionVariables.EDITOR}";
      };
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

  readline = {
    enable = true;
    extraConfig = ''
      set editing-mode vi
    '';
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

  zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableVteIntegration = true;
    syntaxHighlighting.enable = true;
    historySubstringSearch.enable = true;

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
    '';

    initExtraBeforeCompInit = ''
      autoload -Uz zcalc
      autoload -Uz edit-command-line

      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line
      bindkey -v '^?' backward-delete-char

      setopt globdots
      setopt autopushd
    '';
  };
}
