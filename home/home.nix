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
    };

    packages = with pkgs; [
      age-plugin-yubikey
      boxes
      direnv
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

    ] ++ (lib.optionals hostPlatform.isLinux [
      bluetuith
      bubblewrap
      cfspeedtest
      dosfstools
      exfatprogs
      gnuapl
      httpie
      libva-utils
      monero-cli
      ntfs3g
      powertop
      strace

    ] ++ (lib.optionals (!isHeadless) [
      brightnessctl
      cozette
      easyeffects
      feishin
      fira-code
      fira-code-symbols
      gimp-with-plugins
      google-chrome
      grim
      gtk3
      httpie-desktop
      hyprland-qtutils
      hyprpicker
      hyprshot
      hyprswitch
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
      rose-pine-hyprcursor
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
      wireshark
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
      discord
      element-desktop
      kotatogram-desktop
      monero-gui
      whatsapp-for-linux
      zoom-us

    ]))) ++ (lib.optionals hostPlatform.isDarwin ([
      mas

    ] ++ (with brewCasks; [
      (hashOverride firefox "sha256-yJ7pq896NVSVmn0tsKWnSL464sMNfBcLh53hDkYSdgI=")
      linearmouse
    ])));

    pointerCursor = {
      gtk.enable = hostPlatform.isLinux && !isHeadless;
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
