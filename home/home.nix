{ config
, osConfig
, pkgs
, system
, lib
, root
, user
, inputs
, stateVersion
, ...
}:

let
  inherit (osConfig.nixpkgs) hostPlatform;

  homeDir = "/home/${user.login}";

  nix-misc = inputs.nix-misc.packages.${hostPlatform.system};
  ragenix = inputs.ragenix.packages.${hostPlatform.system}.default;

  editor = lib.getBin (pkgs.writeShellScript "editor" ''
    exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -ct $@
  '');

  hashOverride = drv: hash: (drv.overrideAttrs (old: {
    src = pkgs.fetchurl {
      inherit hash;
      url = old.src.url;
    };
  }));
in
{
  home = {
    inherit stateVersion;

    # XXX: https://github.com/nix-community/home-manager/issues/4826
    activation.batCache = lib.mkForce (lib.hm.dag.entryAfter [ "linkGeneration" ] '''');

    username = user.login;

    packages = with pkgs; [
      age-plugin-yubikey
      spek
      tamsyn
      nix-misc.git-fuzzy
      adwaita-icon-theme
      bacon
      boxes
      breeze-icons
      btop
      comma
      direnv
      dos2unix
      duf
      fasd
      fd
      ffmpeg
      file
      fira-code
      fira-code-symbols
      gh
      hack-font
      hicolor-icon-theme
      htop
      hyperfine
      iperf
      jq
      killall
      ldns
      libnotify
      lsof
      material-design-icons
      mediainfo
      miniserve
      mkpasswd
      mpc_cli
      ncdu
      nix-inspect
      nix-output-monitor
      nix-tree
      nixd
      nmap
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      nurl
      onefetch
      pandoc
      patchutils
      procs
      python3
      rage
      ragenix
      rclone
      ripgrep
      scc
      seaweedfs
      shell-gpt
      shellcheck
      tcpdump
      tig
      toilet
      unzip
      weechat
      wget
      whois
      zellij
      zip
    ] ++ lib.optionals (hostPlatform.isLinux) [
      bemenu
      bluetuith
      bubblewrap
      cfspeedtest
      discord
      dosfstools
      easyeffects
      element-desktop-wayland
      exfatprogs
      firefox-wayland
      gimp-with-plugins
      gnuapl
      google-chrome
      grim
      gtk3
      httpie
      httpie-desktop
      hyprpicker
      imv
      libreoffice-fresh
      libva-utils
      monero
      nomacs
      ntfs3g
      obs-studio
      obs-studio-plugins.droidcam-obs
      obs-studio-plugins.obs-pipewire-audio-capture
      obs-studio-plugins.wlrobs
      pamixer
      pavucontrol
      playerctl
      powertop
      remmina
      simple-scan
      slurp
      strace
      swappy
      swaylock
      telegram-desktop
      vial
      vlc
      whatsapp-for-linux
      wireshark
      wl-clipboard
      wlr-randr
      xdg-user-dirs
      xdg-utils
      xorg.xeyes
      xorg.xkill
      yubikey-manager
      zoom-us
    ] ++ lib.optionals (hostPlatform.isDarwin) (with brewCasks; [
      (hashOverride alacritty "sha256-xxziP8NlxNBG3ipIFh0mIypXNMUZ5rX/P1XGAlgmD2A=")
      (hashOverride firefox "sha256-yJ7pq896NVSVmn0tsKWnSL464sMNfBcLh53hDkYSdgI=")
      (hashOverride google-chrome "sha256-nJnpIOaOWFST0SoS0Ip6RcaiMuwTZOhT0VNRC79tvQM=")
      linearmouse
    ]);

    file = {
      dnsCheck = {
        source = "${root}/conf/bin/dnscheck.sh";
        target = ".local/bin/dnscheck";
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

      emacs-ayu-dark = {
        source = "${root}/conf/emacs/ayu-dark-theme.el";
        target = ".emacs.d/ayu-dark-theme.el";
      };
    };

    sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

    sessionVariables = {
      # General
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";

      # Wayland
      MOZ_ENABLE_WAYLAND = "1";
      XKB_DEFAULT_OPTIONS = "caps:escape";

      EDITOR = editor;
    };

    shellAliases = {
      cat = "bat";
      g = "git";
      open = "xdg-open";
      ovpn = "openvpn3";
      rlf = "readlink -f";
      tf = "terraform";
      zc = "zcalc -r";
      zj = "zellij";
      b2 = "buck2";
      bz = "bazel";
      ms = "miniserve -HWqrgzl --readme --index index.html";

      noti = "noti ";
      doas = "doas ";
      sudo = "sudo ";

      sc = "systemctl";
      uc = "systemctl --user";
      jc = "journalctl";
      jcu = "journalctl --user";
      udc = "udisksctl";
      lc = "launchctl";

      vi = "${editor} -t";
      vim = "${editor} -t";

      rscp = "rsync -ahvP";
    };
  } // lib.optionalAttrs (hostPlatform.isLinux) {
    pointerCursor = {
      gtk.enable = true;
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 16;
    };
  };
}
