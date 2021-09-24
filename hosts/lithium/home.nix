{ config, pkgs, lib, root, ... }:

let
  username = "nixpower";
  name = "Armeen Mahdian";
  email = "mahdianarmeen@gmail.com";
  self = config.home-manager.users."${username}";

  conf = "${root}/conf";
  restart-wm = "${pkgs.xmonad-with-packages}/bin/xmonad --restart";
in
{
  home-manager.users."${username}" = rec {
    home = let
      symlink = self.lib.file.mkOutOfStoreSymlink;
      seafile = "${self.home.homeDirectory}/.seafile/Seafile";
      files = "${seafile}/mylib";
      shared = "${seafile}/common";
    in {
      inherit username;
      homeDirectory = "/home/${username}";
      stateVersion = lib.mkForce "21.05";
  
      file.lock = {
        target = ".local/bin/lock";
        executable = true;
        text = ''
          #!${pkgs.bash}/bin/bash
          ${pkgs.playerctl}/bin/playerctl -a pause
          ${config.security.wrapperDir}/physlock
          #${pkgs.vbetool}/bin/vbetool dpms off
        '';
      };

      sessionVariables = {
        PATH = "$PATH:$HOME/.local/bin";
        LESSHISTFILE = "${self.xdg.cacheHome}/less/history";
        ANDROID_SDK_ROOT = "${self.xdg.dataHome}/android";
        ANDROID_SDK_HOME = "${self.xdg.dataHome}/android";
        ANDROID_EMULATOR_HOME = "${self.xdg.dataHome}/android";
        CUDA_CACHE_PATH = "${self.xdg.cacheHome}/nv";
      };

      file.shared.source = symlink shared;
      file.files.source = symlink files;
      file.dl.source = symlink "${files}/dl";
      file.media.source = symlink "${files}/media";
      file.music.source = symlink "${files}/music";
      file.docs.source = symlink "${files}/docs";
      file.desktop.source = symlink "${files}/desktop";
      file.templates.source = symlink "${files}/templates";
      file.ss.source = symlink "${files}/ss";
  
      packages = with pkgs; [
        mullvad-vpn
        home-manager
        mathematica
        discord
        gimp-with-plugins
        trash-cli
        element-desktop
        mosh
        monero
        qdirstat
        texlive.combined.scheme-full
        pandoc
        evince
        vbetool
        bash
        tree
        nix-tree
        caffeine-ng
        file
        killall
        torbrowser
        niv
        tcpdump
        arandr
        pamixer
        pavucontrol
        ldns
        mpc_cli
        xautolock
        xclip
        xorg.xinit
        xorg.xev
        xorg.xdpyinfo
        xorg.xkill
        zoom-us
        bottom
        python3
        nodejs
        usbutils
        pciutils
        fasd
        libnotify
        wget
        keepassxc
        ledger-live-desktop
        ntfs3g
        tdesktop
        whatsapp-for-linux
        neofetch
        vlc
        streamlink
        unrar
        unzip
        direnv
        miraclecast
        weechat
        whois
        ripgrep
        cloc
        nmap
        iperf
        seafile-client
        seafile-shared
        speedtest-cli
        wineWowPackages.stable
        (winetricks.override { wine = wineWowPackages.stable; })
        ranger
        mediainfo
        highlight
        atool
        w3m
        xmobar
        yubico-pam
        yubikey-manager-qt
        yubikey-personalization
        yubikey-personalization-gui
        yubioath-desktop
        gnumake
        rsync
        strace
        glib-networking
        tamsyn
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        fira-code
        fira-code-symbols
        font-awesome-ttf
        libreoffice-fresh
        gparted
        parted
        exfatprogs
        dosfstools
        mtools
        sl
        tpm-tools
        rng-tools
        trousers # TODO
        gksu
        efibootmgr
        playerctl
        toilet
        figlet
        lolcat
        cowsay
        fortune
        vulkan-loader
        vulkan-tools
        linuxPackages.cpupower
        gnome.gtk
        xdg-user-dirs
      ];
    };

    xdg = {
      enable = true;

      userDirs = {
        enable = true;
        desktop = "\$HOME/desktop";
        documents = "\$HOME/docs";
        download = "\$HOME/dl";
        music = "\$HOME/music";
        pictures = "\$HOME/media";
        publicShare = "\$HOME/shared";
        templates = "\$HOME/templates";
        videos = "\$HOME/media";
      };

      configFile = {
        "xmobar".source = "${conf}/xmobar";
        "flameshot".source = "${conf}/flameshot";
        "discord/settings.json".source = "${conf}/discord/settings.json";
        "zsh/.p10k.zsh".source = "${conf}/zsh/p10k.zsh";
      };
    };

    xsession = {
      enable = true;

      scriptPath = ".config/xsession";
      profilePath = ".config/xprofile";

      profileExtra = "autorandr --load default";
  
      pointerCursor = {
        package = pkgs.vanilla-dmz;
        name = "Vanilla-DMZ-AA";
        size = 16;
      };
  
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = "${root}/conf/xmonad/xmonad.hs";
      };
    };
  
    xresources = {
      path = "${self.xdg.configHome}/xresources";
      
      extraConfig = builtins.readFile (
        pkgs.fetchFromGitHub {
          owner = "dracula";
          repo = "xresources";
          rev = "8de11976678054f19a9e0ec49a48ea8f9e881a05";
          sha256 = "p8/E7nA+A5niKsqkO7/c3iDkINyTPAgWf91nMK2XlYs=";
        } + "/Xresources"
      );
    };

    fonts.fontconfig.enable = lib.mkForce true;

    services = {
      flameshot.enable = true;
      playerctld.enable = true;

      emacs = {
        enable = false;
        client.enable = true;
        client.arguments = [ "-c" "-n" ];
      };

      dunst = {
        enable = true;
        configFile = "${root}/conf/dunst/dunstrc";
      };
  
      screen-locker = {
        enable = true;
        inactiveInterval = 5;
        lockCmd = with home; "${homeDirectory}/${file.lock.target}";
      };
  
      grobi = {
        enable = true;
        executeAfter = [ restart-wm ];
      };
  
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        pinentryFlavor = "emacs";
      };
      
      mpd = {
        enable = true;
        network.startWhenNeeded = true;
      };
    };
  
    programs = {
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
            "restart-wm" = restart-wm;
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
        userEmail = email;
        userName = name;
  
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

        dotDir = "${builtins.baseNameOf self.xdg.configHome}/zsh";
        history.path = "${self.xdg.cacheHome}/zsh/history";

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
    };
  };
}
