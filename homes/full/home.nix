{ config, sys, pkgs, lib, root, user, inputs, ... }:

let
  symlink = config.lib.file.mkOutOfStoreSymlink;

  home = "/home/${user.login}";

  files = "${home}/files";
  common = "${home}/common";

  # TODO: Factor this out along with nixpkgs.hostPlatform
  nix-misc = inputs.nix-misc.packages.x86_64-linux;
in
{
  username = user.login;
  homeDirectory = home;
  stateVersion = sys.system.stateVersion;

  packages =
    ## Lang Specific ##
    (with pkgs; [
      gnuapl
      nil
      shellcheck
    ]) ++

    ## CLI Utils ##
    (with nix-misc; [
      git-fuzzy
    ]) ++

    (with pkgs; [
      bottom
      btop
      cloc
      comma
      direnv
      dos2unix
      fasd
      fd
      ffmpeg
      file
      gh
      htop
      hyperfine
      joshuto
      jq
      killall
      libnotify
      libva-utils
      lsof
      mediainfo
      miniserve
      ncdu
      nix-tree
      nurl
      onefetch
      pandoc
      patchutils
      powertop
      procs
      pstree
      ripgrep
      scc
      sl
      sops
      strace
      tcpdump
      tldr
      tmux
      unrar
      unzip
      xplr
      zellij
      zip
    ]) ++

    ## Networking ##
    (with pkgs; [
      remmina
      bluetuith
      croc
      gping
      iperf
      ipfs
      ldns
      mosh
      nmap
      scrcpy
      speedtest-cli
      w3m
      wget
      whois
      wireshark
      xh
    ]) ++

    ## Privacy and Security ##
    (with pkgs; [
      bubblewrap
      keepassxc
      ledger-live-desktop
      monero
      monero-gui
      tor-browser-bundle-bin
      usbguard
      veracrypt
      yubikey-manager
      yubikey-manager-qt
      yubikey-personalization
      yubikey-personalization-gui
      yubioath-flutter
    ]) ++

    ## Desktop Environment ##
    (with pkgs; [
      firefox-wayland
      google-chrome

      audacity
      gimp-with-plugins
      inkscape
      libreoffice-fresh

      bemenu
      grim
      imv
      nomacs
      simple-scan
      slurp
      swappy
      swaylock
      wl-clipboard
      wlr-randr

      tamsyn
      xdg-user-dirs
      xdg-utils
      xorg.xeyes
      xorg.xkill

      breeze-icons
      gnome.adwaita-icon-theme

      fira-code
      fira-code-symbols
      hicolor-icon-theme
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji

      gtk3

      vial
    ]) ++

    ## Windows ##
    (with pkgs; [
      ntfs3g
      dosfstools
      efibootmgr
      exfatprogs
    ]) ++

    ## Media ##
    (with pkgs; [
      easyeffects
      mpc_cli
      pamixer
      pavucontrol
      streamlink
      vlc
      yt-dlp
      playerctl
    ]) ++

    ## Communication ##
    (with pkgs; [
      discord-canary
      element-desktop
      weechat
      zoom-us
    ]);

  file = {
    desktop.source = symlink "${files}/desktop";
    dl.source = symlink "${files}/dl";
    docs.source = symlink "${files}/docs";
    media.source = symlink "${files}/media";
    music.source = symlink "${files}/music";
    ss.source = symlink "${files}/ss";
    templates.source = symlink "${files}/templates";

    dnsCheck = {
      source = "${root}/conf/bin/dnscheck.sh";
      target = ".local/bin/dnscheck";
      executable = true;
    };

    lesskey = {
      target = ".lesskey";
      text = ''
        #env
        LESSHISTFILE=${config.xdg.cacheHome}/less/history

        #command
        / forw-search ^W
      '';
    };

    emacs-ayu-dark = {
      source = "${root}/conf/emacs/ayu-dark-theme.el";
      target = ".emacs.d/ayu-dark-theme.el";
    };
  };

  sessionPath = [ "${home}/.local/bin" ];

  sessionVariables = {
    # General
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";

    # Wayland
    MOZ_ENABLE_WAYLAND = "1";
    XKB_DEFAULT_OPTIONS = "caps:escape";

    EDITOR = lib.getBin (pkgs.writeShellScript "editor" ''
      exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -ct $@
    '');
  };

  shellAliases = {
    cat = "bat";
    diff = "delta";
    g = "git";
    open = "xdg-open";
    ovpn = "openvpn3";
    rlf = "readlink -f";
    tf = "terraform";
    zc = "zcalc -r";
    zl = "zellij";
    bz = "bazel";
    ms = "miniserve -HWqrgzl --readme --index index.html";

    noti = "noti ";
    doas = "doas ";
    sudo = "doas ";

    sc = "systemctl";
    jc = "journalctl";
    uc = "systemctl --user";
    udc = "udisksctl";

    vi = "$EDITOR -t";
    vim = "$EDITOR -t";

    rscp = "rsync -ahvP";
  };
}
