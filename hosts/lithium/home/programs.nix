{ config, pkgs, lib, root, user, misc, ... }:

{
  home-manager.enable = true;
  aria2.enable = true;
  emacs.enable = true;
  htop.enable = true;
  command-not-found.enable = true;
  dircolors.enable = true;
  feh.enable = true;
  noti.enable = true;
  ncmpcpp.enable = true;
  gpg.enable = true;
  mpv.enable = true;
  autojump.enable = true;
  fzf.enable = true;

  autorandr = {
    enable = true;

    profiles = {
      "default" = {
        fingerprint = {
          DP-0 = "00ffffffffffff0004724707fe4f1104291e0104b53e22783bad65ad50459f250e50542308008140818081c081009500b300d1c001014dd000a0f0703e80302035006d552100001a565e00a0a0a02950302035006d552100001e000000fd00283ca0a03c010a202020202020000000fc0056473238304b0a2020202020200197020339f150010304121305141f9007025d5e5f60612309070783010000e200d56d030c0010003878200060010203e305e301e6060701606045023a801871382d40582c45006d552100001e011d007251d01e206e2855006d552100001e8c0ad08a20e02d10103e96006d55210000180000000000000000000000000000000011";
          DP-2 = "00ffffffffffff001e6d095b0e8c0200091b0104b53c22789e3035a7554ea3260f50542108007140818081c0a9c0d1c08100010101014dd000a0f0703e803020650c58542100001a286800a0f0703e800890650c58542100001a000000fd00383d1e8738000a202020202020000000fc004c4720556c7472612048440a2001fb0203117144900403012309070783010000023a801871382d40582c450058542100001e565e00a0a0a029503020350058542100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c8";
        };

        config = {
          DP-0 = {
            enable = true;
            primary = false;
            crtc = 1;
            mode = "3840x2160";
            position = "0x0";
            rate = "60.00";
          };

          DP-2 = {
            enable = true;
            primary = true;
            crtc = 0;
            mode = "3840x2160";
            position = "3840x0";
            rate = "60.00";
          };
        };
      };
    };

    hooks = {
      postswitch = {
        "restart-wm" = misc.restart-wm;
      };  
    };
  };

  nix-index = {
    enable = true;
    enableZshIntegration = false;
    enableBashIntegration = false;
    enableFishIntegration = false;
  };

  chromium = {
    enable = true;
    extensions = [
      { id = "gfapcejdoghpoidkfodoiiffaaibpaem"; } # Dracula Theme
      { id = "nkbihfbeogaeaoehlefnkodbefgpgknn"; } # MetaMask
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # uBlock Origin
      { id = "ohnjgmpcibpbafdlkimncjhflgedgpam"; } # 4chanX
      { id = "hfjbmagddngcpeloejdejnfgbamkjaeg"; } # Vimium-C
      { id = "kbmfpngjjgdllneeigpgjifpgocmfgmb"; } # RES
      { id = "kcgpggonjhmeaejebeoeomdlohicfhce"; } # Cookie Remover
      { 
        id = "dcpihecpambacapedldabdbpakmachpb";
        updateUrl = "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/updates.xml";
      } # Bypass Paywalls
      
    ];
  };

  direnv = {
    enable = true;
    nix-direnv.enable = true;
    nix-direnv.enableFlakes = true;
  };

  exa = {
    enable = true;
    enableAliases = true;
  };

  gh = {
    enable = true;
    gitProtocol = "ssh";
  };

  git = {
    enable = true;
    userEmail = user.email;
    userName = user.name;

    delta = {
      enable = true;
      options = {
        syntax-theme = "Dracula";
        line-numbers = true;
      };
    };

    includes = [ { path = "${root}/conf/git/general.inc"; } ];
  };

  neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    extraConfig = ''
      set number
      set hidden
      set shell=bash
      set cmdheight=2
      set nocompatible
      set shortmess+=c
      set updatetime=300
      set background=dark
      set foldmethod=marker
      set signcolumn=yes
      set nobackup nowritebackup
      set tabstop=2 shiftwidth=2 expandtab
      set tagrelative
      set tags^=./.git/tags;
      set mouse=a
    '';
  };

  urxvt = {
    enable = true;
    fonts = [ "xft:Tamsyn:size=10" ];
    scroll.bar.enable = false;

    extraConfig = {
      boldFont = "xft:Tamsyn:bold:size=10";
      letterSpace = 0;
      lineSpace = 0;
      geometry = "92x24";
      internalBorder = 12;
      cursorBlink = true;
      cursorUnderline = false;
      saveline = 2048;
      scrollBar = false;
      scrollBar_right = false;
      urgentOnBell = true;
      depth = 24;
      scrollTtyOutput = false;
      scrollWithBuffer = true;
      scrollTtyKeypress = true;
      secondaryScreen = 1;
      secondaryScroll = 0;
      url-launcher = "chromium";
      perl-ext-common = "default,matcher";
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

      setopt globdots
    '';

    shellAliases = {
      "hm" = "home-manager";
      "userctl" = "systemctl --user";
      "zc" = "zcalc -r";
      "open" = "xdg-open";
      "sudo" = "sudo ";
      "noti" = "noti ";
    };

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
