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
  hypruse = import ./hypruse.nix { inherit lib pkgs; };
  strace-macos = inputs.strace-macos.packages.${hostPlatform.system}.default;

  editor = lib.getBin (pkgs.writeShellScript "editor" ''
    if [ -z "''${WAYLAND_DISPLAY+x}" ]; then
      exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -ct $@
    else
      exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -c $@
    fi
  '');

  # OpenChamber goes through a wrapper so that the GUI and the OpenCode server
  # it spawns inherit the same environment as the terminal `opencode`: the
  # age-encrypted OpenCode env file, the Wayland clipboard library path, and
  # this config's OpenCode CLI rather than the one upstream would bundle.
  openchamber = pkgs.writeShellApplication {
    name = "openchamber";

    text = let
      clipboardLibraryPath = lib.optionalString hostPlatform.isLinux
        (lib.makeLibraryPath [ pkgs.wayland ]);
    in ''
      set -a
      # shellcheck disable=SC1091
      source "${config.age.secrets.opencode-env.path}"
      set +a

      # OpenCode's native clipboard backend dlopen()s libwayland-client.so.0
      # at runtime; the prebuilt binary does not carry it.
      export LD_LIBRARY_PATH="${clipboardLibraryPath}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

      export OPENCODE_BINARY="${lib.getExe config.programs.opencode.package}"

      exec ${lib.getExe pkgs.openchamber-desktop} "$@"
    '';
  };

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
        age-plugin-yubikey
        android-tools
        ast-grep
        bitwarden-cli
        boxes
        croc
        dos2unix
        duf
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
        jsonnet-language-server
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
      ] ++ lib.optionals (!(hostPlatform.isLinux && hostPlatform.isAarch64)) [
        # nixpkgs marks the Swift compiler this needs as broken on
        # aarch64-linux, so it's only buildable on the other Linux/macOS hosts.
        age-plugin-se
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
        grim
        gtk3
        hyprland-qtutils
        hyprpicker
        hyprshot
        hypruse
        kdePackages.breeze-icons
        kdePackages.dolphin
        libnotify
        libreoffice
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
        virt-manager
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
        openchamber
        telegram-desktop
        zoom-us
      ] ++ lib.optionals (lib.meta.availableOn hostPlatform pkgs.vial) [
        # Vial is x86_64-linux only upstream.
        vial
      ];

      darwin-gui = [ bluesnooze ] ++  (with brewCasks; [
        (autoraiseapp.overrideAttrs (old: {
          src = pkgs.fetchurl {
            url = lib.lists.head old.src.urls;
            hash = "sha256-8iBhcguxQ9jJ+pbOPFHN0LzH4YIP9frjxAv4MANrsG8=";
          };
        }))
        betterdisplay
        bettertouchtool
        gimp
        (google-chrome.overrideAttrs (old: {
          src = pkgs.fetchurl {
            url = lib.lists.head old.src.urls;
            hash = "sha256-dw9tHUkBfdgOMH8ftB3zjRLrNrcJR8V4C/tHWSxM0dc=";
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
        monero-wallet
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

      OPENCODE_ENABLE_EXA = 1;

      # Some runtimes ship their own CA bundle and would keep trusting the
      # revoked eMudhra intermediates after the October 2026 CA compromise.
      # Point them at the system trust store, which carries the blacklist.
      # https://emudhra.we-have-a-problem.com/
      SSL_CERT_FILE = if hostPlatform.isLinux
        then "/etc/ssl/certs/ca-certificates.crt"
        else "${pkgs.cacert}/etc/ssl/certs/ca-certificates.crt";
      UV_NATIVE_TLS = "1";
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
