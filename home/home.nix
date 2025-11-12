{ config
, isHeadless
, enableSocial
, pkgs
, lib
, user
, inputs
, stateVersion
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;

  hyprland-qtutils = inputs.hyprland-qtutils.packages.${hostPlatform.system}.default;
  hyprshell = inputs.hyprshell.packages.${hostPlatform.system}.hyprshell;

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
      dnscheck = {
        source = ../conf/bin/dnscheck.sh;
        target = ".local/bin/dnscheck";
        executable = true;
      };

      sshrc = {
        source = ../conf/ssh/rc;
        target = ".ssh/rc";
        executable = true;
      };

      aider = {
        source = ../conf/aider/aider.conf.yml;
        target = ".aider.conf.yml";
      };
    };

    packages = with pkgs; [
      age-plugin-yubikey
      bitwarden-cli
      boxes
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

    ] ++ lib.optionals hostPlatform.isLinux ([
      bluetuith
      bubblewrap
      cfspeedtest
      gnuapl
      httpie
      libva-utils
      miraclecast
      monero-cli
      powertop
      strace

    ] ++ (lib.optionals (!isHeadless) [
      brightnessctl
      bruno
      cozette
      easyeffects
      #feishin
      fira-code
      fira-code-symbols
      gimp-with-plugins
      google-chrome
      gparted
      grim
      gtk3
      httpie-desktop
      hyprland-qtutils
      hyprpicker
      hyprshell
      hyprshot
      kdePackages.breeze-icons
      kdePackages.dolphin
      libnotify
      libreoffice-fresh
      meld
      moonlight-qt
      nomacs
      noto-fonts
      noto-fonts-cjk-sans
      obs-studio
      obs-studio-plugins.obs-pipewire-audio-capture
      obs-studio-plugins.wlrobs
      open-in-mpv
      pamixer
      pavucontrol
      playerctl
      pulseaudio
      remmina
      satty
      scrcpy
      shikane
      simple-scan
      slurp
      spek
      swaylock
      tamsyn
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
      xorg.xeyes
      xorg.xkill
      yubikey-manager

    ] ++ (lib.optionals enableSocial [
      aider-chat
      claude-code
      discord
      element-desktop
      kotatogram-desktop
      monero-gui
      wasistlos
      zoom-us

    ]))) ++ (lib.optionals hostPlatform.isDarwin ([
      mas
      google-chrome

    ] ++ (with brewCasks; [
      linearmouse
    ])));

    pointerCursor = {
      enable = hostPlatform.isLinux && !isHeadless;
      gtk.enable = true;
      hyprcursor.enable = true;
      x11.enable = true;

      package = pkgs.rose-pine-cursor;
      name = "Rose Pine";
      size = 16;
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
