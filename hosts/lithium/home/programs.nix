{ config, pkgs, lib, root, user, misc, ... }:

{
  aria2.enable = true;
  autojump.enable = true;
  command-not-found.enable = true;
  dircolors.enable = true;
  emacs.enable = true;
  feh.enable = true;
  fzf.enable = true;
  home-manager.enable = true;
  htop.enable = true;
  mpv.enable = true;
  ncmpcpp.enable = true;
  noti.enable = true;

  autorandr = {
    enable = true;

    profiles = {
      "default" = {
        fingerprint = {
          DP-0 = "00ffffffffffff0004724707fe4f1104291e0103803e22782aad65ad50459f250e5054bfef80714f8140818081c081009500b300d1c04dd000a0f0703e80303035006d552100001a565e00a0a0a02950302035006d552100001e000000fd00283c1ea03c000a202020202020000000fc0056473238304b0a202020202020019002034df151010304121305141f100706025d5e5f606123090707830100006d030c001000383c20006001020367d85dc401788003681a00000101283ce6e305e301e40f008001e6060701606045023a801871382d40582c45006d552100001e8c0ad08a20e02d10103e96006d55210000180000000000000000000000000000a6";
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

  bottom = {
    enable = true;
    settings = {

    };
  };

  chromium = {
    enable = true;
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

  exa = {
    enable = true;
    enableAliases = true;
  };

  gh = {
    enable = false;
  };

  gpg = {
    enable = true;
    homedir = "${config.xdg.dataHome}/gnupg";
    settings = {
      pinentry-mode = "loopback";
    };
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

  urxvt =
    let
      font = "Tamsyn";
      size = "9";
    in
      {
        enable = true;
        fonts = [ "xft:${font}:size=${size}" ];
        scroll.bar.enable = false;

        extraConfig = {
          boldFont = "xft:${font}:bold:size=${size}";
          cursorBlink = false;
          cursorUnderline = false;
          depth = 24;
          geometry = "92x24";
          internalBorder = 12;
          letterSpace = 0;
          lineSpace = 0;
          perl-ext-common = "default,matcher";
          saveline = 2048;
          scrollTtyKeypress = true;
          scrollTtyOutput = false;
          scrollWithBuffer = true;
          secondaryScreen = 1;
          secondaryScroll = 0;
          urgentOnBell = true;
          url-launcher = "chromium";
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
      "doas" = "doas ";
      "hm" = "home-manager";
      "noti" = "noti ";
      "open" = "xdg-open";
      "sudo" = "doas ";
      "userctl" = "systemctl --user";
      "vi" = "$EDITOR";
      "vim" = "$EDITOR";
      "zc" = "zcalc -r";
      "cat" = "bat";
      "diff" = "delta";
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
