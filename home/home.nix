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

  nix-misc = inputs.nix-misc.packages.x86_64-linux;
  ragenix = inputs.ragenix.packages.x86_64-linux.default;

  editor = lib.getBin (pkgs.writeShellScript "editor" ''
    exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -ct $@
  '');
in
{
  home = {
    inherit stateVersion;

    # XXX: https://github.com/nix-community/home-manager/issues/4826
    activation.batCache = lib.mkForce (lib.hm.dag.entryAfter [ "linkGeneration" ] '''');

    username = user.login;

    packages = with pkgs; [
      #git-fuzzy
      #ueberzugpp
      adwaita-icon-theme
      age-plugin-yubikey
      bacon
      boxes
      breeze-icons
      btop
      comma
      direnv
      dos2unix
      dosfstools
      duf
      element-desktop-wayland
      fasd
      fd
      ffmpeg
      file
      fira-code
      fira-code-symbols
      gh
      gtk3
      hack-font
      hicolor-icon-theme
      htop
      httpie
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
      monero
      mpc_cli
      multimarkdown
      ncdu
      nix-inspect
      nix-output-monitor
      nix-tree
      nixd
      nmap
      nomacs
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      ntfs3g
      nurl
      onefetch
      pandoc
      patchutils
      procs
      python3
      rage
      ragenix
      rclone
      remmina
      ripgrep
      s3cmd
      scc
      seaweedfs
      shell-gpt
      shellcheck
      spek
      tamsyn
      tcpdump
      telegram-desktop
      tig
      toilet
      unzip
      weechat
      wget
      whois
      wireshark
      wl-clipboard
      xdg-user-dirs
      xdg-utils
      xorg.xeyes
      xorg.xkill
      yubikey-manager
      zellij
      zip
    ] ++ lib.optionals (hostPlatform.isLinux) [
      bluetuith
      bemenu
      bubblewrap
      cfspeedtest
      discord
      easyeffects
      exfatprogs
      firefox-wayland
      google-chrome
      gimp-with-plugins
      gnuapl
      grim
      httpie-desktop
      hyprpicker
      imv
      libreoffice-fresh
      libva-utils
      obs-studio
      obs-studio-plugins.droidcam-obs
      obs-studio-plugins.obs-pipewire-audio-capture
      obs-studio-plugins.wlrobs
      pamixer
      pavucontrol
      playerctl
      powertop
      simple-scan
      slurp
      strace
      swappy
      swaylock
      vial
      vlc
      whatsapp-for-linux
      wlr-randr
      zoom-us
    ];

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
      jc = "journalctl";
      uc = "systemctl --user";
      udc = "udisksctl";

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
