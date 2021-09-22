{ config, pkgs, lib, root, ... }:

let
  restart-wm = "${pkgs.xmonad-with-packages}/bin/xmonad --restart";
  self = config.home-manager.users.nixpower;
in
{
  home-manager.users.nixpower = rec {
    home = let
      symlink = self.lib.file.mkOutOfStoreSymlink;
      seafile = "${self.home.homeDirectory}/.seafile/Seafile";
    in {
      username = "nixpower";
      homeDirectory = "/home/nixpower";
      stateVersion = lib.mkForce "21.05";
  
      file.lock = {
        target = "bin/lock";
        executable = true;
        text = ''
          #!${pkgs.bash}/bin/bash
          ${pkgs.playerctl}/bin/playerctl -a pause
          ${config.security.wrapperDir}/physlock
          #${pkgs.vbetool}/bin/vbetool dpms off
        '';
      };

      sessionVariables = {
        HISTFILE = "$XDG_CACHE_HOME/zsh_history";
      };

      file.seafile.source = symlink seafile;
      file.dl.source = symlink "${seafile}/mylib/dl";
      file.media.source = symlink "${seafile}/mylib/media";
      file.music.source = symlink "${seafile}/mylib/music";
      file.docs.source = symlink "${seafile}/mylib/docs";
      file.ss.source = symlink "${seafile}/mylib/ss";
  
      packages = with pkgs; [
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
        discord
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
        ddcutil
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
      ];
    };

    xdg = {
      enable = true;

      configFile = {
        "xmobar".source = "${root}/conf/xmobar";
      };
    };

    fonts.fontconfig.enable = lib.mkForce true;

    xsession = {
      enable = true;
  
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
      extraConfig = builtins.readFile (
        pkgs.fetchFromGitHub {
          owner = "dracula";
          repo = "xresources";
          rev = "8de11976678054f19a9e0ec49a48ea8f9e881a05";
          sha256 = "p8/E7nA+A5niKsqkO7/c3iDkINyTPAgWf91nMK2XlYs=";
        } + "/Xresources"
      );
    };
  
    services = {
      flameshot.enable = true;
      playerctld.enable = true;
      dunst.enable = true; # TODO: theme
  
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
      
      emacs = {
        enable = true;
      };
  
      mpd = {
        enable = true;
        network.startWhenNeeded = true;
      };
    };
  
    programs = {
      home-manager.enable = true;
      emacs.enable = true;
      htop.enable = true;
      command-not-found.enable = true;
      dircolors.enable = true;
      feh.enable = true;
      noti.enable = true;
      ncmpcpp.enable = true;
      gpg.enable = true;
      mpv.enable = true;

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
        userEmail = "mahdianarmeen@gmail.com";
        userName = "Armeen Mahdian";
  
        delta = {
          enable = true;
          options = {
            syntax-theme = "Dracula";
            line-numbers = true;
          };
        };
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
  
      fzf = {
        enable = true;
        enableZshIntegration = true;
      };
  
      zsh = {
        enable = true;
        enableAutosuggestions = true;
        enableCompletion = true;
        autocd = true;
        defaultKeymap = "viins";
        dotDir = ".config/zsh";
        shellAliases = {
          "hm" = "home-manager";
          "userctl" = "systemctl --user";
          "open" = "xdg-open";
          "sudo" = "sudo ";
          "noti" = "noti ";
        };
      };
    };
  };
}
