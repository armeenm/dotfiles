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
      theme = "Dracula";
    };
    themes = {
      dracula = builtins.readFile (pkgs.fetchFromGitHub
        {
          owner = "dracula";
          repo = "sublime";
          rev = "26c57ec282abcaa76e57e055f38432bd827ac34e";
          sha256 = "019hfl4zbn4vm4154hh3bwk6hm7bdxbr1hdww83nabxwjn99ndhv";
        } + "/Dracula.tmTheme");
    };
  };

  broot = {
    enable = true;
    modal = true;
  };

  chromium = {
    enable = true;
    #package = pkgs.chromiumDev;
    extensions = [
      { id = "aapbdbdomjkkjkaonfhkkikfgjllcleb"; } # Google Translate
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # uBlock Origin
      { id = "cledppeceojodgghbbkaciochldmpdfk"; } # Twitter Media Assist
      { id = "dmkamcknogkgcdfhhbddcghachkejeap"; } # Keplr
      { id = "dneaehbmnbhcippjikoajpoabadpodje"; } # Old Reddit Redirect
      { id = "gebbhagfogifgggkldgodflihgfeippi"; } # Return YouTube Dislike
      { id = "hfjbmagddngcpeloejdejnfgbamkjaeg"; } # Vimium C
      { id = "kbmfpngjjgdllneeigpgjifpgocmfgmb"; } # RES
      { id = "kcgpggonjhmeaejebeoeomdlohicfhce"; } # Cookie Remover
      { id = "nibjojkomfdiaoajekhjakgkdhaomnch"; } # IPFS Companion
      { id = "nkbihfbeogaeaoehlefnkodbefgpgknn"; } # MetaMask
    ];
  };

  direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  emacs = {
    enable = true;
    package = pkgs.emacsPgtkGcc;
    extraConfig = builtins.readFile "${root}/conf/emacs/default.el";
    extraPackages = epkgs: with epkgs; [
      avy
      cloc
      company
      counsel
      dap-mode
      deadgrep
      direnv
      frames-only-mode
      general
      hl-todo
      ivy
      projectile
      smooth-scrolling
      swiper
      which-key

      clojure-mode
      haskell-mode
      meson-mode
      nix-mode
      rust-mode
      solidity-mode
      typescript-mode

      gruvbox-theme

      evil
      evil-collection

      lsp-haskell
      lsp-ivy
      lsp-mode
      lsp-treemacs
      lsp-ui

      magit
      magit-delta
      magit-todos

      treemacs
      treemacs-evil
      treemacs-projectile

      undo-fu-session
      undo-fu

      yasnippet
      yasnippet-snippets
    ];
  };

  exa = {
    enable = true;
    enableAliases = true;
  };

  foot = {
    enable = true;
    server.enable = true;

    settings = {
      main = {
        term = "xterm-256color";
        dpi-aware = "yes";
      };

      mouse = {
        hide-when-typing = "yes";
      };

      colors = {
        background = "282828";
        foreground = "ebdbb2";
        regular0 = "282828";
        regular1 = "cc241d";
        regular2 = "98971a";
        regular3 = "d79921";
        regular4 = "458588";
        regular5 = "b16286";
        regular6 = "689d6a";
        regular7 = "a89984";
        bright0 = "928374";
        bright1 = "fb4934";
        bright2 = "b8bb26";
        bright3 = "fabd2f";
        bright4 = "83a598";
        bright5 = "d3869b";
        bright6 = "8ec07c";
        bright7 = "ebdbb2";
      };
    };
  };

  gh = {
    enable = false;
  };

  gpg = {
    enable = true;
    homedir = "${config.xdg.dataHome}/gnupg";
    #settings = {
    #  pinentry-mode = "loopback";
    #};
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
      st = "status";
      sw = "switch";
      wt = "worktree";
    };

    delta = {
      enable = true;
      options = {
        syntax-theme = "Dracula";
        line-numbers = true;
      };
    };

    includes = [{ path = "${root}/conf/git/general.inc"; }];
  };

  ion = {
    enable = true;
  };

  mako = {
    enable = true;
    extraConfig = ''
      [mode=do-not-disturb]
      invisible=1
    '';
  };

  mpv = {
    enable = true;
    config = {
      gpu-api = "vulkan";
      #gpu-context = "wayland";
      gpu-context = "x11vk";
      hwdec = "nvdec-copy";
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

  starship = {
    enable = true;
    settings = {
      add_newline = false;
      character = {
        success_symbol = "[>](bold green)";
        error_symbol = "[x](bold red)";
        vicmd_symbol = "[<](bold green)";
      };
    };
  };

  waybar = {
    enable = true;
    settings = {
      mainBar = {
        output = [ "DP-2" ];
        modules-right = [
          "idle_inhibitor"
          "pulseaudio"
          "network"
          "cpu"
          "memory"
          "temperature"
          "backlight"
          "keyboard-state"
          "sway/language"
          "clock"
          "tray"
        ];
      };
    };
  };

  zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;

    autocd = true;
    defaultKeymap = "viins";

    dotDir = "${builtins.baseNameOf config.xdg.configHome}/zsh";
    history.path = "${config.xdg.cacheHome}/zsh/history";

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
