{ config, osConfig, pkgs, lib, root, user, inputs, ... }:

let
  homeDir = "/home/${user.login}";

  nix-misc = inputs.nix-misc.packages.x86_64-linux;
  ragenix = inputs.ragenix.packages.x86_64-linux.default;

  editor = lib.getBin (pkgs.writeShellScript "editor" ''
    exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -ct $@
  '');
in
{
  home = {
    # XXX: https://github.com/nix-community/home-manager/issues/4826
    activation.batCache = lib.mkForce (lib.hm.dag.entryAfter [ "linkGeneration" ] '''');

    username = user.login;
    stateVersion = osConfig.system.stateVersion;

    packages = with pkgs; [
      #git-fuzzy
      age-plugin-yubikey
      bacon
      bemenu
      bluetuith
      boxes
      breeze-icons
      btop
      bubblewrap
      cfspeedtest
      comma
      direnv
      discord
      dos2unix
      dosfstools
      duf
      easyeffects
      exfatprogs
      fasd
      fd
      ffmpeg
      file
      fira-code
      fira-code-symbols
      firefox-wayland
      gh
      gimp-with-plugins
      gnome.adwaita-icon-theme
      gnuapl
      google-chrome
      grim
      gtk3
      hack-font
      hicolor-icon-theme
      htop
      httpie
      httpie-desktop
      hyperfine
      hyprpicker
      imv
      iperf
      jq
      killall
      ldns
      libnotify
      libreoffice-fresh
      libva-utils
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
      obs-studio
      obs-studio-plugins.droidcam-obs
      obs-studio-plugins.obs-pipewire-audio-capture
      obs-studio-plugins.wlrobs
      onefetch
      pamixer
      pandoc
      patchutils
      pavucontrol
      playerctl
      powertop
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
      simple-scan
      slurp
      strace
      swappy
      swaylock
      tamsyn
      tcpdump
      telegram-desktop
      tig
      toilet
      ueberzugpp
      unzip
      vial
      vlc
      weechat
      wget
      whatsapp-for-linux
      whois
      wireshark
      wl-clipboard
      wlr-randr
      xdg-user-dirs
      xdg-utils
      xorg.xeyes
      xorg.xkill
      yubikey-manager
      zellij
      zip
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

    pointerCursor = {
      gtk.enable = true;
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
      size = 16;
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
  };
}
