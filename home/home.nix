{ config
, isHeadless
, enableSocial
, cursorColor
, cursorSize
, pkgs
, lib
, user
, inputs
, stateVersion
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;
  inherit (inputs.nix-misc.packages.${hostPlatform.system}) launchk;

  hyprland-qtutils = inputs.hyprland-qtutils.packages.${hostPlatform.system}.default;
  hyprshell = inputs.hyprshell.packages.${hostPlatform.system}.hyprshell;
  strace-macos = inputs.strace-macos.packages.${hostPlatform.system}.default;

  editor = lib.getBin (pkgs.writeShellScript "editor" ''
    if [ -z "''${WAYLAND_DISPLAY+x}" ]; then
      exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -ct $@
    else
      exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -c $@
    fi
  '');

  hashOverride = drv: hash: (drv.overrideAttrs (old: {
    src = pkgs.fetchurl {
      inherit hash;
      url = old.src.url;
    };
  }));

in {
  home = {
    inherit stateVersion;
    extraOutputsToInstall = [ "devdoc" "doc" "info" ];
    homeDirectory = lib.mkOverride 500 "/home/${user.login}";
    preferXdgDirectories = true;
    shell.enableZshIntegration = true;
    username = lib.mkOverride 500 user.login;

    file = {
      bin = {
        source = ../conf/bin;
        target = ".local/bin";
        recursive = true;
      };

      sshrc = {
        source = ../conf/ssh/rc;
        target = ".ssh/rc";
        executable = true;
      };
    };

    packages = with pkgs; let
      all-cli = [
        age-plugin-se
        age-plugin-yubikey
        android-tools
        bitwarden-cli
        boxes
        croc
        dos2unix
        duf
        fasd
        fastmod
        fd
        ffmpeg
        file
        git-filter-repo
        glow
        hexyl
        hyperfine
        iperf
        jq
        killall
        ldns
        lsof
        mediainfo
        miniserve
        mkpasswd
        ncdu
        nix-inspect
        nix-output-monitor
        nix-tree
        nixd
        nmap
        nurl
        onefetch
        patchutils
        procs
        python3
        rage
        ragenix
        rclone
        scc
        shellcheck
        tcpdump
        toilet
        unzip
        wget
        whois
        zip
      ];

      linux-cli = [
        bluetuith
        bubblewrap
        cfspeedtest
        gnuapl
        httpie
        libva-utils
        miraclecast
        powertop
        strace
      ] ++ lib.optionals enableSocial [
        monero-cli
      ];

      darwin-cli = [
        launchk
        mas
        strace-macos
      ];

      all-gui = [
        bruno
        cozette
        fira-code
        fira-code-symbols
        meld
        moonlight-qt
        nerd-fonts.hack
        noto-fonts
        noto-fonts-cjk-sans
        remmina
        spek
        tamsyn
        yubikey-manager
      ] ++ lib.optionals enableSocial [
        claude-code
        element-desktop
        signal-desktop
      ];

      linux-gui = [
        brightnessctl
        easyeffects
        evince
        feishin
        gimp-with-plugins
        google-chrome
        gparted
        grim
        gtk3
        hyprland-qtutils
        hyprpicker
        hyprshell
        hyprshot
        kdePackages.breeze-icons
        kdePackages.dolphin
        libnotify
        libreoffice-fresh
        nomacs
        obs-studio
        obs-studio-plugins.obs-pipewire-audio-capture
        obs-studio-plugins.wlrobs
        pamixer
        pavucontrol
        playerctl
        pulseaudio
        satty
        scrcpy
        shikane
        simple-scan
        slurp
        swaylock
        vial
        vlc
        wdisplays
        wl-clipboard
        wl-screenrec
        wlogout
        wlr-randr
        woomer
        xdg-user-dirs
        xdg-utils
        xeyes
        xkill
      ] ++ lib.optionals enableSocial [
        karere
      ];

      darwin-gui = [ bluesnooze ] ++  (with brewCasks; [
        (autoraiseapp.overrideAttrs (old: {
          src = pkgs.fetchurl {
            url = lib.lists.head old.src.urls;
            hash = "sha256-x2qThyvm9ZS+GLm6ScGuq2kv0/8WZJdNms9rKYFsbB0=";
          };
        }))
        betterdisplay
        bettertouchtool
        gimp
        (google-chrome.overrideAttrs (old: {
          src = pkgs.fetchurl {
            url = lib.lists.head old.src.urls;
            hash = "sha256-pVR0W1yGCxUo64VpUmmzEFSedYZXkF2l0gRIog2HkEw=";
          };
        }))
        libreoffice
        linearmouse
        mozilla-vpn
        obs
        vlc
        (windows-app.overrideAttrs (old: {
          unpackPhase = ''
            xar -xf $src
            for pkg in $(cat Distribution | grep -oE "#.+\.pkg" | sed -e "s/^#//" -e "s/$/\/Payload/"); do
              if [ -f $pkg ]; then
                zcat $pkg | cpio -i
              fi
            done
          '';
        }))

      ] ++ lib.optionals enableSocial [
        # monero-wallet
        telegram
        (whatsapp.overrideAttrs (old: {
          # Rename source to "whatsapp.zip".
          src = pkgs.runCommand "whatsapp.zip" {} ''
            ln -s ${old.src} $out
          '';
        }))
        zoom
      ]);

      cli = all-cli ++ (if hostPlatform.isLinux then linux-cli else darwin-cli);
      gui = all-gui ++ (if hostPlatform.isLinux then linux-gui else darwin-gui);
      final = cli ++ lib.optionals (!isHeadless) gui;

    in final;

    pointerCursor = {
      enable = hostPlatform.isLinux && !isHeadless;
      gtk.enable = true;
      hyprcursor.enable = true;
      x11.enable = true;

      package = pkgs.symlinkJoin {
        name = "rose-pine-cursor-combo";
        paths = let
          xcursor = pkgs.rose-pine-cursor;
          hyprcursor = inputs.rose-pine-hyprcursor.packages.${hostPlatform.system}.default;
          hyprcursor-final = if cursorColor != null
                             then hyprcursor.override { color = cursorColor; }
                             else hyprcursor;
        in [ hyprcursor-final xcursor ];
      };

      name = "BreezeX-RoséPine";
      size = if cursorSize == null then 32 else cursorSize;
    };

    sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

    sessionVariables = {
      # General
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";

      # Wayland
      MOZ_ENABLE_WAYLAND = 1;
      NIXOS_OZONE_WL = 1;
      XKB_DEFAULT_OPTIONS = "caps:escape";
      _JAVA_AWT_WM_NONREPARENTING = 1;

      EDITOR = editor;
    };

    shellAliases = {
      lg = "ls -laahg";
      ms = "miniserve -HWqrgzl --readme --index index.html";

      noti = "noti ";
      sudo = "sudo ";

      vi = "${editor}";
      vim = "${editor}";
    } // lib.optionalAttrs hostPlatform.isLinux {
      doas = "doas ";
    };
  };
}
