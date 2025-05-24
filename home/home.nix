{ config
, osConfig
, isHeadless
, enableSocial
, pkgs
, lib
, root
, user
, inputs
, stateVersion
, ...
}:

let
  inherit (osConfig.nixpkgs) hostPlatform;

  hyprland-qtutils = inputs.hyprland-qtutils.packages.${hostPlatform.system}.default;
  nix-misc = inputs.nix-misc.packages.${hostPlatform.system};

  editor = lib.getBin (pkgs.writeShellScript "editor" ''
    exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -ct $@
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
    username = lib.mkOverride 500 user.login;

    file = {
      dnscheck = {
        source = "${root}/conf/bin/dnscheck.sh";
        target = ".local/bin/dnscheck";
        executable = true;
      };

      sshrc = {
        source = "${root}/conf/ssh/rc";
        target = ".ssh/rc";
        executable = true;
      };

      lesskey = {
        target = ".lesskey";
        text = ''
          #env

          #command
          / forw-search ^W
        '';
      };
    };

    packages = with pkgs; [
      age-plugin-yubikey
      boxes
      comma
      direnv
      dos2unix
      duf
      fasd
      fd
      ffmpeg
      file
      git-filter-repo
      hexyl
      htop
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
      nix-misc.git-fuzzy
      nix-output-monitor
      nix-tree
      nmap
      nurl
      onefetch
      patchutils
      procs
      python3
      rage
      ragenix
      rclone
      ripgrep
      scc
      shellcheck
      tcpdump
      tig
      toilet
      tty-clock
      unzip
      wget
      whois
      zip

    ] ++ (lib.optionals (hostPlatform.isLinux) [
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
      bluetuith
      brightnessctl
      cozette
      easyeffects
      feishin
      fira-code
      fira-code-symbols
      firefox-wayland
      gimp-with-plugins
      google-chrome
      grim
      gtk3
      httpie-desktop
      hyprland-qtutils
      hyprpicker
      hyprshot
      imv
      libnotify
      libreoffice-fresh
      material-design-icons
      moonlight-qt
      nomacs
      noto-fonts
      noto-fonts-cjk-sans
      obs-studio
      obs-studio-plugins.obs-pipewire-audio-capture
      obs-studio-plugins.wlrobs
      pamixer
      pavucontrol
      playerctl
      pulseaudio
      remmina
      rose-pine-hyprcursor
      satty
      simple-scan
      slurp
      spek
      swaylock
      tamsyn
      vial
      vlc
      wireshark
      wl-clipboard
      wlr-randr
      xdg-user-dirs
      xdg-utils
      xorg.xeyes
      xorg.xkill
      yubikey-manager

    ] ++ (lib.optionals enableSocial [
      discord
      element-desktop
      monero-gui
      telegram-desktop
      whatsapp-for-linux
      zoom-us

    ]))) ++ (lib.optionals (hostPlatform.isDarwin) ([
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
      MOZ_ENABLE_WAYLAND = "1";
      NIXOS_OZONE_WL = "1";
      XKB_DEFAULT_OPTIONS = "caps:escape";
      _JAVA_AWT_WM_NONREPARENTING = 1;

      EDITOR = editor;
    };

    shellAliases = {
      b2 = "buck2";
      bz = "bazel";
      ibz = "ibazel";
      cat = "bat";
      g = "git";
      ms = "miniserve -HWqrgzl --readme --index index.html";
      rlf = "readlink -f";
      tf = "terraform";
      zc = "zcalc -r";
      zj = "zellij";
      lg = "ls -laahg";

      noti = "noti ";
      sudo = "sudo ";

      vi = "${editor} -t";
      vim = "${editor} -t";

      rscp = "rsync -ahvP";
    } // lib.optionalAttrs (hostPlatform.isLinux) {
      doas = "doas ";
      open = "xdg-open";

      jc = "journalctl";
      jcu = "journalctl --user";
      sc = "systemctl";
      uc = "systemctl --user";
      udc = "udisksctl";
    } // lib.optionalAttrs (hostPlatform.isDarwin) {
      lc = "launchctl";
    };
  };
}
