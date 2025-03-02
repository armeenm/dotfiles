{ config
, osConfig
, isHeadless
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

  nix-misc = inputs.nix-misc.packages.${hostPlatform.system};
  ragenix = inputs.ragenix.packages.${hostPlatform.system}.default;
  hyprland-qtutils = inputs.hyprland-qtutils.packages.${hostPlatform.system}.default;

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
    username = lib.mkOverride 500 user.login;
    homeDirectory = lib.mkOverride 500 "/home/${user.login}";

    # XXX: https://github.com/nix-community/home-manager/issues/4826
    activation.batCache = lib.mkForce (lib.hm.dag.entryAfter [ "linkGeneration" ] '''');

    packages = with pkgs; [
      adwaita-icon-theme
      age-plugin-yubikey
      bacon
      boxes
      kdePackages.breeze-icons
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
      git-filter-repo
      google-chrome
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
      nix-misc.git-fuzzy
      nix-output-monitor
      nix-tree
      nixd
      nmap
      noto-fonts
      noto-fonts-cjk-sans
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
      spek
      tamsyn
      tcpdump
      tig
      toilet
      unzip
      weechat
      wget
      whois
      zip
    ] ++ (lib.optionals (hostPlatform.isLinux) [
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
      pamixer
      pavucontrol
      playerctl
      powertop
    ] ++ (lib.optionals (!isHeadless) [
      bemenu
      discord
      easyeffects
      element-desktop
      firefox-wayland
      #gimp-with-plugins
      grim
      gtk3
      httpie-desktop
      hyprland-qtutils
      hyprpicker
      imv
      libreoffice-fresh
      nomacs
      obs-studio
      obs-studio-plugins.obs-pipewire-audio-capture
      obs-studio-plugins.wlrobs
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
    ])) ++ (lib.optionals (hostPlatform.isDarwin) ([
      mas
    ] ++ (with brewCasks; [
      (hashOverride firefox "sha256-yJ7pq896NVSVmn0tsKWnSL464sMNfBcLh53hDkYSdgI=")
      linearmouse
    ])));

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
      NIXOS_OZONE_WL = "1";

      EDITOR = editor;
    };

    shellAliases = {
      b2 = "buck2";
      bz = "bazel";
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

  } // lib.optionalAttrs (hostPlatform.isLinux) {
    pointerCursor = {
      gtk.enable = !isHeadless;
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 16;
    };
  };
}
