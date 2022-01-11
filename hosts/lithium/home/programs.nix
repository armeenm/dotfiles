{ config, pkgs, lib, root, user, ... }:

{
  aria2.enable = true;
  autojump.enable = true;
  command-not-found.enable = false;
  dircolors.enable = true;
  fzf.enable = true;
  home-manager.enable = true;
  nix-index.enable = true;
  noti.enable = true;

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
      dracula = builtins.readFile (pkgs.fetchFromGitHub {
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
    package = pkgs.chromiumDev;
    extensions = [
      { id = "gfapcejdoghpoidkfodoiiffaaibpaem"; } # Dracula Theme
      { id = "nkbihfbeogaeaoehlefnkodbefgpgknn"; } # MetaMask
      { id = "dmkamcknogkgcdfhhbddcghachkejeap"; } # Keplr
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # uBlock Origin
      { id = "ohnjgmpcibpbafdlkimncjhflgedgpam"; } # 4chanX
      { id = "hfjbmagddngcpeloejdejnfgbamkjaeg"; } # Vimium-C
      { id = "cledppeceojodgghbbkaciochldmpdfk"; } # Twitter Media Assist
      { id = "dneaehbmnbhcippjikoajpoabadpodje"; } # Old Reddit Redirect
      { id = "kbmfpngjjgdllneeigpgjifpgocmfgmb"; } # RES
      { id = "kcgpggonjhmeaejebeoeomdlohicfhce"; } # Cookie Remover
      { id = "nibjojkomfdiaoajekhjakgkdhaomnch"; } # IPFS Companion
      { id = "aapbdbdomjkkjkaonfhkkikfgjllcleb"; } # Google Translate
      { id = "gebbhagfogifgggkldgodflihgfeippi"; } # Return YouTube Dislike
      { 
        id = "dcpihecpambacapedldabdbpakmachpb";
        updateUrl = "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/manifest.json";
      } # Bypass Paywalls
    ];
  };

  direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  emacs = {
    enable = true;
    package = pkgs.emacsPgtk;
  };

  exa = {
    enable = true;
    enableAliases = true;
  };

  feh = {
    enable = true;
    keybindings = {
      prev_img = "h";
      next_img = "l";
      zoom_in = "k";
      zoom_out = "j";
    };
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
      aa = "add -A";
      br = "branch";
      ci = "commit";
      co = "checkout";
      st = "status";
    };

    delta = {
      enable = true;
      options = {
        syntax-theme = "Dracula";
        line-numbers = true;
      };
    };

    includes = [ { path = "${root}/conf/git/general.inc"; } ];
  };

  mako = {
    enable = true;
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

  waybar = {
    enable = true;
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

    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.4.0";
          sha256 = "037wz9fqmx0ngcwl9az55fgkipb745rymznxnssr3rx9irb6apzg";
        };
      }
    ];
  };
}
